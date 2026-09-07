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
