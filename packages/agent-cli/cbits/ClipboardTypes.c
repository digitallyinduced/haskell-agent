/* Inspect advertised representations without requesting clipboard payloads.
 * In particular, do not ask macOS to coerce ordinary text into an image or
 * a Finder file list: those failed coercions can take seconds.
 */
/* Include only the required subframework headers. The umbrella headers pull
 * in unrelated Carbon filesystem declarations and collide with libuuid's
 * headers in the Nix development environment.
 */
#include <ApplicationServices/../Frameworks/HIServices.framework/Headers/Pasteboard.h>
#include <CoreServices/../Frameworks/LaunchServices.framework/Headers/UTType.h>
#include <pthread.h>
#include <stdint.h>

int agent_cli_clipboard_flavor_may_contain_images(CFStringRef flavor)
{
    if (flavor == NULL)
        return -1;
    return UTTypeConformsTo(flavor, CFSTR("public.image")) ||
           UTTypeConformsTo(flavor, CFSTR("public.file-url")) ||
           CFEqual(flavor, CFSTR("NSFilenamesPboardType")) ||
           CFEqual(flavor, CFSTR("com.apple.pasteboard.promised-file-url")) ||
           CFEqual(flavor, CFSTR("com.apple.pasteboard.promised-file-content-type")) ||
           CFEqual(flavor, CFSTR("com.apple.pasteboard.finder-node"));
}

/* Return 0 only after a successful inspection, 1 for image/file candidates,
 * and -1 on failure so callers can retain their existing clipboard readers.
 * Each call owns its PasteboardRef; no shared mutable state or AppKit main
 * thread is required.
 */
int agent_cli_pasteboard_may_contain_images(CFStringRef pasteboard_name)
{
    PasteboardRef clipboard = NULL;
    ItemCount item_count = 0;
    int result = 0;

    if (pasteboard_name == NULL ||
        PasteboardCreate(pasteboard_name, &clipboard) != noErr)
        return -1;
    PasteboardSynchronize(clipboard);
    if (PasteboardGetItemCount(clipboard, &item_count) != noErr) {
        CFRelease(clipboard);
        return -1;
    }

    for (ItemCount item_index = 1; item_index <= item_count; ++item_index) {
        PasteboardItemID item_identifier;
        CFArrayRef flavors = NULL;
        if (PasteboardGetItemIdentifier(clipboard, item_index,
                                       &item_identifier) != noErr ||
            PasteboardCopyItemFlavors(clipboard, item_identifier,
                                     &flavors) != noErr) {
            result = -1;
            break;
        }
        for (CFIndex flavor_index = 0;
             flavor_index < CFArrayGetCount(flavors); ++flavor_index) {
            CFStringRef flavor = CFArrayGetValueAtIndex(flavors, flavor_index);
            if (agent_cli_clipboard_flavor_may_contain_images(flavor)) {
                result = 1;
                break;
            }
        }
        CFRelease(flavors);
        if (result != 0)
            break;
    }
    CFRelease(clipboard);
    return result;
}

int agent_cli_clipboard_may_contain_images(void)
{
    return agent_cli_pasteboard_may_contain_images(kPasteboardClipboard);
}

/* Cached pasteboard for the focus-driven image tip. PasteboardSynchronize on
 * a retained reference reports kPasteboardModified, which is the Carbon
 * equivalent of NSPasteboard.changeCount. Classification inspects advertised
 * types only: no payload coercion and no AppKit linkage.
 */
static pthread_mutex_t clipboard_snapshot_lock = PTHREAD_MUTEX_INITIALIZER;
static PasteboardRef clipboard_snapshot_ref = NULL;
static uint64_t clipboard_snapshot_generation = 0;
static int clipboard_snapshot_generation_initialized = 0;

static int flavor_is_file_url(CFStringRef flavor)
{
    return flavor != NULL &&
           (UTTypeConformsTo(flavor, CFSTR("public.file-url")) ||
            CFEqual(flavor, CFSTR("public.file-url")) ||
            CFEqual(flavor, CFSTR("NSFilenamesPboardType")));
}

static int flavor_is_raster(CFStringRef flavor)
{
    return flavor != NULL &&
           (UTTypeConformsTo(flavor, CFSTR("public.png")) ||
            UTTypeConformsTo(flavor, CFSTR("public.tiff")) ||
            UTTypeConformsTo(flavor, CFSTR("public.jpeg")) ||
            CFEqual(flavor, CFSTR("public.png")) ||
            CFEqual(flavor, CFSTR("public.tiff")) ||
            CFEqual(flavor, CFSTR("public.jpeg")));
}

static PasteboardRef clipboard_snapshot_pasteboard(void)
{
    if (clipboard_snapshot_ref == NULL &&
        PasteboardCreate(kPasteboardClipboard, &clipboard_snapshot_ref) != noErr)
        return NULL;
    return clipboard_snapshot_ref;
}

static uint64_t clipboard_snapshot_sync_generation(PasteboardRef clipboard)
{
    PasteboardSyncFlags flags = PasteboardSynchronize(clipboard);
    if (!clipboard_snapshot_generation_initialized) {
        clipboard_snapshot_generation = 1;
        clipboard_snapshot_generation_initialized = 1;
    } else if (flags & kPasteboardModified) {
        clipboard_snapshot_generation += 1;
    }
    return clipboard_snapshot_generation;
}

static int clipboard_snapshot_classify(PasteboardRef clipboard, int *out_has_image)
{
    ItemCount item_count = 0;
    int has_file_url = 0;
    int has_raster = 0;

    if (PasteboardGetItemCount(clipboard, &item_count) != noErr)
        return 0;

    for (ItemCount item_index = 1; item_index <= item_count; ++item_index) {
        PasteboardItemID item_identifier;
        CFArrayRef flavors = NULL;
        if (PasteboardGetItemIdentifier(clipboard, item_index,
                                       &item_identifier) != noErr ||
            PasteboardCopyItemFlavors(clipboard, item_identifier,
                                     &flavors) != noErr)
            return 0;
        for (CFIndex flavor_index = 0;
             flavor_index < CFArrayGetCount(flavors); ++flavor_index) {
            CFStringRef flavor = CFArrayGetValueAtIndex(flavors, flavor_index);
            if (flavor_is_file_url(flavor))
                has_file_url = 1;
            if (flavor_is_raster(flavor))
                has_raster = 1;
        }
        CFRelease(flavors);
        if (has_file_url && has_raster)
            break;
    }

    *out_has_image = has_raster && !has_file_url;
    return 1;
}

/* Return 1 and write the monotonic generation; 0 when the pasteboard is
 * unavailable. The generation is metadata-only.
 */
int agent_cli_clipboard_change_count(unsigned long long *out_count)
{
    PasteboardRef clipboard;
    uint64_t generation;

    if (out_count == NULL)
        return 0;
    pthread_mutex_lock(&clipboard_snapshot_lock);
    clipboard = clipboard_snapshot_pasteboard();
    if (clipboard == NULL) {
        pthread_mutex_unlock(&clipboard_snapshot_lock);
        return 0;
    }
    generation = clipboard_snapshot_sync_generation(clipboard);
    pthread_mutex_unlock(&clipboard_snapshot_lock);
    *out_count = generation;
    return 1;
}

/* Return 1 and write generation plus whether a pasteable raster is advertised
 * without file-URL types. Finder copies advertise a file-icon raster next to
 * file URLs; Ctrl+V routes those through path handling, so they must not fire
 * the image-paste tip. Return 0 when inspection fails.
 */
int agent_cli_clipboard_image_snapshot(unsigned long long *out_count,
                                       int *out_has_image)
{
    PasteboardRef clipboard;
    uint64_t generation;
    int has_image = 0;

    if (out_count == NULL || out_has_image == NULL)
        return 0;
    pthread_mutex_lock(&clipboard_snapshot_lock);
    clipboard = clipboard_snapshot_pasteboard();
    if (clipboard == NULL) {
        pthread_mutex_unlock(&clipboard_snapshot_lock);
        return 0;
    }
    generation = clipboard_snapshot_sync_generation(clipboard);
    if (!clipboard_snapshot_classify(clipboard, &has_image)) {
        pthread_mutex_unlock(&clipboard_snapshot_lock);
        return 0;
    }
    pthread_mutex_unlock(&clipboard_snapshot_lock);
    *out_count = generation;
    *out_has_image = has_image;
    return 1;
}
