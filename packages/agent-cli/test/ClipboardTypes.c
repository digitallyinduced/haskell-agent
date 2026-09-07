/* Standalone macOS regression tests. Only unique named pasteboards are used;
 * the user's general clipboard is never read or modified.
 */
#include <ApplicationServices/../Frameworks/HIServices.framework/Headers/Pasteboard.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

int agent_cli_pasteboard_may_contain_images(CFStringRef name);

static unsigned int promise_requests = 0;

static OSStatus supply_promised_data(PasteboardRef clipboard,
                                    PasteboardItemID item,
                                    CFStringRef flavor, void *context)
{
    (void)clipboard;
    (void)item;
    (void)flavor;
    (void)context;
    ++promise_requests;
    return badPasteboardFlavorErr;
}

static void put_flavor(PasteboardRef clipboard, unsigned int item,
                       CFStringRef flavor)
{
    const UInt8 bytes[] = "https://example.org/document";
    CFDataRef data = CFDataCreate(NULL, bytes, sizeof(bytes) - 1);
    assert(data != NULL);
    assert(PasteboardPutItemFlavor(clipboard, (PasteboardItemID)(uintptr_t)item,
                                  flavor, data, 0) == noErr);
    CFRelease(data);
}

int main(void)
{
    char name[128];
    snprintf(name, sizeof(name), "org.haskell-agent.clipboard-types-test.%ld",
             (long)getpid());
    CFStringRef clipboard_name =
        CFStringCreateWithCString(NULL, name, kCFStringEncodingUTF8);
    PasteboardRef clipboard = NULL;
    assert(PasteboardCreate(clipboard_name, &clipboard) == noErr);
    assert(PasteboardClear(clipboard) == noErr);
    assert(agent_cli_pasteboard_may_contain_images(clipboard_name) == 0);

    put_flavor(clipboard, 1, CFSTR("public.utf8-plain-text"));
    put_flavor(clipboard, 1, CFSTR("public.url"));
    assert(agent_cli_pasteboard_may_contain_images(clipboard_name) == 0);
    put_flavor(clipboard, 1, CFSTR("public.html"));
    put_flavor(clipboard, 1, CFSTR("public.rtf"));
    assert(agent_cli_pasteboard_may_contain_images(clipboard_name) == 0);

    const CFStringRef image_flavors[] = {
        CFSTR("public.png"), CFSTR("public.jpeg"), CFSTR("public.tiff"),
        CFSTR("public.file-url"), CFSTR("NSFilenamesPboardType"),
        CFSTR("com.apple.pasteboard.promised-file-url"),
        CFSTR("com.apple.pasteboard.promised-file-content-type"),
        CFSTR("com.apple.pasteboard.finder-node")
    };
    for (size_t index = 0;
         index < sizeof(image_flavors) / sizeof(image_flavors[0]); ++index) {
        assert(PasteboardClear(clipboard) == noErr);
        put_flavor(clipboard, 1, CFSTR("public.utf8-plain-text"));
        put_flavor(clipboard, 2, image_flavors[index]);
        assert(agent_cli_pasteboard_may_contain_images(clipboard_name) == 1);
    }

    assert(PasteboardClear(clipboard) == noErr);
    assert(PasteboardSetPromiseKeeper(clipboard, supply_promised_data, NULL)
           == noErr);
    assert(PasteboardPutItemFlavor(clipboard, (PasteboardItemID)1,
                                  CFSTR("public.png"), NULL, 0) == noErr);
    assert(agent_cli_pasteboard_may_contain_images(clipboard_name) == 1);
    assert(promise_requests == 0);
    assert(agent_cli_pasteboard_may_contain_images(NULL) == -1);

    assert(PasteboardClear(clipboard) == noErr);
    CFRelease(clipboard);
    CFRelease(clipboard_name);
    puts("Clipboard metadata: 13 cases passed; no promised payload requested.");
    return 0;
}
