#import <Foundation/NSArray.h>
#import <Foundation/NSData.h>
#import <Foundation/NSDictionary.h>
#import <Foundation/NSJSONSerialization.h>
#import <Foundation/NSNull.h>
#import <Foundation/NSValue.h>
#import <Foundation/NSString.h>
#import <CoreFoundation/CFNumber.h>
#include <JavaScriptCore/JavaScript.h>
#include <mach/mach.h>
#include <mach-o/dyld.h>
#include <poll.h>
#include <unistd.h>
#include <atomic>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <thread>
#include <vector>
#include <deque>
#include <unordered_set>

// One owner thread enters JSC. A separate watchdog never touches a JS value.
// RSS is sampled, not a hard allocation limit; overshoot depends on allocation rate.
static constexpr size_t messageLimit = 16 * 1024 * 1024;
static JSStringRef jsString(NSString *value) {
    std::vector<unichar> characters(value.length);
    [value getCharacters:characters.data() range:NSMakeRange(0, value.length)];
    return JSStringCreateWithCharacters(characters.data(), characters.size());
}
static NSString *nativeString(JSContextRef context, JSValueRef value) {
    JSStringRef string = JSValueToStringCopy(context, value, nullptr);
    if (!string) return @"JavaScript conversion failed";
    NSString *result = [[NSString alloc] initWithCharacters:JSStringGetCharactersPtr(string) length:JSStringGetLength(string)];
    JSStringRelease(string);
    return result;
}
static JSValueRef stringValue(JSContextRef context, NSString *value) {
    JSStringRef string = jsString(value);
    JSValueRef result = JSValueMakeString(context, string);
    JSStringRelease(string);
    return result;
}
static void writeLine(NSString *line) {
    NSData *data = [line dataUsingEncoding:NSUTF8StringEncoding];
    if (!data || data.length > messageLimit) _exit(74);
    const char *bytes = static_cast<const char *>(data.bytes);
    size_t offset = 0;
    while (offset < data.length) {
        ssize_t count = write(STDOUT_FILENO, bytes + offset, data.length - offset);
        if (count <= 0) _exit(74);
        offset += count;
    }
    if (write(STDOUT_FILENO, "\n", 1) != 1) _exit(74);
}
static void sendObject(id object) {
    NSData *data = [NSJSONSerialization dataWithJSONObject:object options:NSJSONWritingFragmentsAllowed error:nil];
    if (!data) _exit(74);
    writeLine([[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding]);
}
static JSValueRef nativeSend(JSContextRef context, JSObjectRef, JSObjectRef, size_t count, const JSValueRef values[], JSValueRef *) {
    if (count != 1 || !JSValueIsString(context, values[0])) _exit(74);
    // The private sender receives an already serialized string. Encode directly
    // and frame in the same buffer rather than round-tripping through NSString.
    JSStringRef string = JSValueToStringCopy(context, values[0], nullptr);
    if (!string || JSStringGetLength(string) > messageLimit) _exit(74);
    size_t capacity = JSStringGetMaximumUTF8CStringSize(string);
    char local[4096];
    char *bytes = capacity <= sizeof(local) ? local : static_cast<char *>(malloc(capacity));
    if (!bytes) _exit(74);
    size_t length = JSStringGetUTF8CString(string, bytes, capacity);
    JSStringRelease(string);
    if (!length || length - 1 > messageLimit) _exit(74);
    bytes[length - 1] = '\n';
    size_t offset = 0;
    while (offset < length) {
        ssize_t written = write(STDOUT_FILENO, bytes + offset, length - offset);
        if (written <= 0) _exit(74);
        offset += written;
    }
    if (bytes != local) free(bytes);
    return JSValueMakeUndefined(context);
}
static double monotonicMilliseconds() {
    return std::chrono::duration<double, std::milli>(std::chrono::steady_clock::now().time_since_epoch()).count();
}
static JSValueRef nativeNow(JSContextRef context, JSObjectRef, JSObjectRef, size_t, const JSValueRef[], JSValueRef *) {
    return JSValueMakeNumber(context, monotonicMilliseconds());
}
static JSValueRef discardCheckOutput(JSContextRef context, JSObjectRef, JSObjectRef, size_t, const JSValueRef[], JSValueRef *) {
    return JSValueMakeUndefined(context);
}
static JSValueRef evaluate(JSContextRef context, NSString *source, JSValueRef *exception) {
    JSStringRef string = jsString(source);
    JSValueRef result = JSEvaluateScript(context, string, nullptr, nullptr, 1, exception);
    JSStringRelease(string);
    return result;
}
static JSValueRef property(JSContextRef context, JSObjectRef object, const char *name) {
    JSStringRef key = JSStringCreateWithUTF8CString(name);
    JSValueRef result = JSObjectGetProperty(context, object, key, nullptr);
    JSStringRelease(key);
    return result;
}
static JSValueRef fromJSON(JSContextRef context, id object) {
    NSData *data = [NSJSONSerialization dataWithJSONObject:object options:NSJSONWritingFragmentsAllowed error:nil];
    JSStringRef string = jsString([[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding]);
    JSValueRef result = JSValueMakeFromJSONString(context, string);
    JSStringRelease(string);
    return result;
}
static bool validParameters(id p) {
    if (![p isKindOfClass:NSDictionary.class] || ![p[@"source"] isKindOfClass:NSString.class] ||
        ![p[@"tools"] isKindOfClass:NSArray.class] || ![p[@"stored_values"] isKindOfClass:NSDictionary.class] ||
        !p[@"image_detail_visible"] ||
        CFGetTypeID((__bridge CFTypeRef)p[@"image_detail_visible"]) != CFBooleanGetTypeID()) return false;
    for (id tool in p[@"tools"]) {
        if ([tool isKindOfClass:NSString.class] && [tool length]) continue;
        if ([tool isKindOfClass:NSDictionary.class] && [tool[@"name"] isKindOfClass:NSString.class] &&
            [tool[@"name"] length] && [tool[@"description"] isKindOfClass:NSString.class]) continue;
        return false;
    }
    return true;
}
int main(int argc, char **argv) {
    @autoreleasepool {
        bool checkOnly = argc == 2 && std::string(argv[1]) == "--check";
        NSMutableArray<NSString *> *paths = [NSMutableArray array];
        if (argc == 1 || checkOnly) {
            uint32_t size = 0;
            _NSGetExecutablePath(nullptr, &size);
            std::vector<char> executable(size);
            if (_NSGetExecutablePath(executable.data(), &size) != 0) return 66;
            NSString *directory = [[@(executable.data()) stringByResolvingSymlinksInPath] stringByDeletingLastPathComponent];
            NSString *resources = [directory stringByAppendingPathComponent:@"../share/agent-code-mode-worker"];
            for (NSString *name in @[@"acorn.js", @"lower-module.js", @"worker.js"]) [paths addObject:[resources stringByAppendingPathComponent:name]];
        } else if (argc == 4) {
            for (int i = 1; i <= 3; i++) [paths addObject:@(argv[i])];
        } else {
            fprintf(stderr, "usage: agent-code-mode-worker [--check | ACORN LOWER_MODULE WORKER_JS]\n");
            return 64;
        }
        std::atomic<bool> stopping(false);
        std::thread watchdog([&] {
            while (!stopping.load()) {
                mach_task_basic_info_data_t info{};
                mach_msg_type_number_t count = MACH_TASK_BASIC_INFO_COUNT;
                if (task_info(mach_task_self(), MACH_TASK_BASIC_INFO, reinterpret_cast<task_info_t>(&info), &count) == KERN_SUCCESS &&
                    info.resident_size > 512ULL * 1024 * 1024) _exit(75);
                std::this_thread::sleep_for(std::chrono::milliseconds(20));
            }
        });
        JSGlobalContextRef parser = JSGlobalContextCreate(nullptr);
        JSContextGroupRef executionGroup = JSContextGroupCreate();
        JSValueRef exception = nullptr;
        for (int i = 0; i < 2; ++i) {
            NSString *source = [NSString stringWithContentsOfFile:paths[i] encoding:NSUTF8StringEncoding error:nil];
            if (!source) _exit(66);
            evaluate(parser, source, &exception);
            if (exception) { fprintf(stderr, "%s\n", [nativeString(parser, exception) UTF8String]); _exit(65); }
        }
        JSObjectRef lower = JSValueToObject(parser, property(parser, JSContextGetGlobalObject(parser), "lowerModule"), nullptr);
        JSValueProtect(parser, lower);
        NSString *bootstrap = [NSString stringWithContentsOfFile:paths[2] encoding:NSUTF8StringEncoding error:nil];
        if (!bootstrap) _exit(66);
        JSStringRef bootstrapSource = jsString(bootstrap);
        if (checkOnly) {
            JSValueRef fixture = stringValue(parser, @"export const answer = await Promise.resolve(42); text(answer);");
            JSValueRef lowered = JSObjectCallAsFunction(parser, lower, nullptr, 1, &fixture, &exception);
            if (exception || !JSValueIsString(parser, lowered)) _exit(65);
            JSGlobalContextRef probe = JSGlobalContextCreateInGroup(executionGroup, nullptr);
            JSValueRef factory = JSEvaluateScript(probe, bootstrapSource, nullptr, nullptr, 1, &exception);
            if (exception || !JSValueIsObject(probe, factory) ||
                !JSObjectIsFunction(probe, JSValueToObject(probe, factory, nullptr))) _exit(65);
            JSValueProtect(probe, factory);
            JSValueRef arguments[] = {
                JSObjectMakeFunctionWithCallback(probe, nullptr, discardCheckOutput),
                fromJSON(probe, @{@"tools": @[], @"stored_values": @{}, @"image_detail_visible": @NO}),
                JSValueMakeNumber(probe, 0),
                JSValueMakeNumber(probe, 0),
                JSObjectMakeFunctionWithCallback(probe, nullptr, nativeNow)
            };
            for (auto argument : arguments) JSValueProtect(probe, argument);
            JSObjectRef driver = JSValueToObject(probe, JSObjectCallAsFunction(probe, JSValueToObject(probe, factory, nullptr), nullptr, 5, arguments, &exception), nullptr);
            if (exception) _exit(65);
            JSValueProtect(probe, driver);
            JSValueRef promise = evaluate(probe, nativeString(parser, lowered), &exception);
            if (exception) _exit(65);
            JSValueProtect(probe, promise);
            JSObjectRef observe = JSValueToObject(probe, property(probe, driver, "observe"), nullptr);
            JSObjectCallAsFunction(probe, observe, driver, 1, &promise, &exception);
            if (exception) _exit(65);
            JSObjectRef stateMethod = JSValueToObject(probe, property(probe, driver, "state"), nullptr);
            JSObjectRef state = JSValueToObject(probe, JSObjectCallAsFunction(probe, stateMethod, driver, 0, nullptr, &exception), nullptr);
            if (exception || !JSValueToBoolean(probe, property(probe, state, "finished"))) _exit(65);
            JSValueUnprotect(probe, promise);
            JSValueUnprotect(probe, driver);
            for (auto argument : arguments) JSValueUnprotect(probe, argument);
            JSValueUnprotect(probe, factory);
            JSGlobalContextRelease(probe);
            JSStringRelease(bootstrapSource);
            JSValueUnprotect(parser, lower);
            JSGlobalContextRelease(parser);
            JSContextGroupRelease(executionGroup);
            stopping.store(true);
            watchdog.join();
            return 0;
        }
        JSGlobalContextRef cell = nullptr;
        JSObjectRef driver = nullptr;
        std::vector<JSObjectRef> methods;
        double nextCall = 0;
        std::unordered_set<std::string> retiredCalls;
        std::deque<std::string> retiredOrder;
        auto call = [&](size_t method, size_t count, const JSValueRef *args) {
            JSValueRef error = nullptr;
            JSValueRef result = JSObjectCallAsFunction(cell, methods[method], driver, count, args, &error);
            if (error) { fprintf(stderr, "worker driver failed: %s\n", [nativeString(cell, error) UTF8String]); _exit(70); }
            return result;
        };
        auto releaseCell = [&] {
            for (auto method : methods) JSValueUnprotect(cell, method);
            methods.clear();
            JSValueUnprotect(cell, driver);
            JSGlobalContextRelease(cell);
            cell = nullptr;
            driver = nullptr;
        };
        auto retireFinished = [&] {
            if (!cell) return;
            JSValueRef snapshot = call(4, 0, nullptr);
            if (JSValueIsNull(cell, snapshot)) return;
            JSObjectRef state = JSValueToObject(cell, snapshot, nullptr);
            nextCall = JSValueToNumber(cell, property(cell, state, "nextCallId"), nullptr);
            if (!JSValueToBoolean(cell, property(cell, state, "finished"))) return;
            JSObjectRef retired = JSValueToObject(cell, property(cell, state, "retired"), nullptr);
            unsigned count = JSValueToNumber(cell, property(cell, retired, "length"), nullptr);
            for (unsigned i = 0; i < count; i++) {
                std::string identifier = [nativeString(cell, JSObjectGetPropertyAtIndex(cell, retired, i, nullptr)) UTF8String];
                retiredCalls.insert(identifier);
                retiredOrder.push_back(identifier);
                if (retiredOrder.size() > 4096) { retiredCalls.erase(retiredOrder.front()); retiredOrder.pop_front(); }
            }
            releaseCell();
        };
        sendObject(@{@"jsonrpc": @"2.0", @"method": @"ready"});
        std::string input;
        size_t scanned = 0;
        bool running = true;
        while (running) {
            @autoreleasepool {
                pollfd descriptor{STDIN_FILENO, POLLIN, 0};
                int available = poll(&descriptor, 1, cell ? 1 : -1);
                if (available < 0) break;
                if (available && (descriptor.revents & (POLLIN | POLLHUP))) {
                    char buffer[65536];
                    ssize_t count = read(STDIN_FILENO, buffer, sizeof(buffer));
                    if (count <= 0) break;
                    input.append(buffer, count);
                }
                size_t newline;
                while ((newline = input.find('\n', scanned)) != std::string::npos) {
                    scanned = 0;
                    retireFinished();
                    if (newline > messageLimit) _exit(74);
                    // A literal NUL is invalid JSON. Reject before using the
                    // UTF-8 C-string JSC API (escaped \u0000 remains valid).
                    if (std::char_traits<char>::find(input.data(), newline, '\0')) _exit(65);
                    NSString *raw = [[NSString alloc] initWithBytes:input.data() length:newline encoding:NSUTF8StringEncoding];
                    input.erase(0, newline + 1);
                    if (!raw) _exit(65);
                    if (cell) {
                        // NSString validates UTF-8; keep ASCII response payloads
                        // on JSC's 8-bit path instead of allocating UTF-16 first.
                        JSStringRef json = JSStringCreateWithUTF8CString(raw.UTF8String);
                        JSValueRef value = JSValueMakeString(cell, json);
                        JSStringRelease(json);
                        JSValueRef status = call(2, 1, &value);
                        if (JSValueIsString(cell, status)) {
                            if (!retiredCalls.erase([nativeString(cell, status) UTF8String])) _exit(65);
                            continue;
                        }
                        if (JSValueIsBoolean(cell, status)) {
                            if (!JSValueToBoolean(cell, status)) _exit(65);
                            continue;
                        }
                        // Only busy exec uses the existing Foundation path.
                        if (!JSValueIsNull(cell, status)) _exit(65);
                    }
                    NSData *data = [raw dataUsingEncoding:NSUTF8StringEncoding];
                    id message = [NSJSONSerialization JSONObjectWithData:data options:0 error:nil];
                    if (![message isKindOfClass:NSDictionary.class] || ![message[@"jsonrpc"] isEqual:@"2.0"]) _exit(65);
                    if ([message[@"method"] isEqual:@"exec"]) {
                        id identifier = message[@"id"] ?: NSNull.null;
                        if (cell || !validParameters(message[@"params"])) {
                            sendObject(@{@"jsonrpc": @"2.0", @"id": identifier, @"error": @{@"code": @(-32000), @"message": cell ? @"this worker accepts exactly one execution" : @"invalid exec parameters"}});
                            continue;
                        }
                        id params = message[@"params"];
                        exception = nullptr;
                        JSValueRef source = stringValue(parser, params[@"source"]);
                        JSValueRef lowered = JSObjectCallAsFunction(parser, lower, nullptr, 1, &source, &exception);
                        if (exception) {
                            sendObject(@{@"jsonrpc": @"2.0", @"id": identifier, @"error": @{@"code": @(-32000), @"message": nativeString(parser, exception)}, @"partial_result": @{@"content": @[]}, @"stored_value_writes": @{}});
                            continue;
                        }
                        JSStringRef program = JSValueToStringCopy(parser, lowered, nullptr);
                        cell = JSGlobalContextCreateInGroup(executionGroup, nullptr);
                        JSObjectRef factory = JSValueToObject(cell, JSEvaluateScript(cell, bootstrapSource, nullptr, nullptr, 1, &exception), nullptr);
                        if (exception) _exit(70);
                        JSValueProtect(cell, factory);
                        JSObjectRef sender = JSObjectMakeFunctionWithCallback(cell, nullptr, nativeSend);
                        JSObjectRef clock = JSObjectMakeFunctionWithCallback(cell, nullptr, nativeNow);
                        // Foundation has validated the envelope. Parse its original JSON in
                        // the pristine cell, avoiding a second native serialization of tools
                        // and stored values. No user code has run in this context yet.
                        JSStringRef requestJSON = JSStringCreateWithUTF8CString(raw.UTF8String);
                        JSValueRef requestValue = JSValueMakeFromJSONString(cell, requestJSON);
                        JSStringRelease(requestJSON);
                        if (!requestValue || !JSValueIsObject(cell, requestValue)) _exit(65);
                        JSValueProtect(cell, requestValue);
                        JSObjectRef requestObject = JSValueToObject(cell, requestValue, nullptr);
                        JSValueRef executionId = message[@"id"] ? property(cell, requestObject, "id") : JSValueMakeNull(cell);
                        JSValueRef arguments[] = {sender, property(cell, requestObject, "params"), executionId, JSValueMakeNumber(cell, nextCall), clock};
                        for (auto argument : arguments) JSValueProtect(cell, argument);
                        JSValueUnprotect(cell, requestValue);
                        driver = JSValueToObject(cell, JSObjectCallAsFunction(cell, factory, nullptr, 5, arguments, &exception), nullptr);
                        if (exception) _exit(70);
                        JSValueProtect(cell, driver);
                        for (auto argument : arguments) JSValueUnprotect(cell, argument);
                        JSValueUnprotect(cell, factory);
                        for (const char *name : {"observe", "fail", "responseRaw", "tick", "state"}) {
                            auto method = JSValueToObject(cell, property(cell, driver, name), nullptr);
                            JSValueProtect(cell, method);
                            methods.push_back(method);
                        }
                        JSValueRef promise = JSEvaluateScript(cell, program, nullptr, nullptr, 1, &exception);
                        JSStringRelease(program);
                        if (exception) { JSValueRef error = stringValue(cell, nativeString(cell, exception)); call(1, 1, &error); }
                        else {
                            JSValueProtect(cell, promise);
                            call(0, 1, &promise);
                            JSValueUnprotect(cell, promise);
                        }
                    } else if ([message[@"id"] isKindOfClass:NSString.class] && (message[@"result"] || message[@"error"])) {
                        std::string responseId = [message[@"id"] UTF8String];
                        if (retiredCalls.erase(responseId)) continue;
                        _exit(65);
                    } else _exit(65);
                }
                scanned = input.size();
                if (scanned > messageLimit) _exit(74);
                if (cell) {
                    JSValueRef now = JSValueMakeNumber(cell, monotonicMilliseconds());
                    call(3, 1, &now);
                    retireFinished();
                }
            }
        }
        if (cell) releaseCell();
        JSStringRelease(bootstrapSource);
        JSValueUnprotect(parser, lower);
        JSGlobalContextRelease(parser);
        JSContextGroupRelease(executionGroup);
        stopping.store(true);
        watchdog.join();
        return 0;
    }
}
