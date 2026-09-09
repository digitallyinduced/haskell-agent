/* Internal platform credential storage. This is not part of the host ABI.
 * Values are opaque credential records; no serialization protocol crosses
 * into Swift. Returned allocations must be released with the matching function.
 */
#include <CoreFoundation/CoreFoundation.h>
#include <Security/Security.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

enum {
    MCP_KEYCHAIN_SUCCESS = 0,
    MCP_KEYCHAIN_NOT_FOUND = 1,
    MCP_KEYCHAIN_INVALID_INPUT = 2,
    MCP_KEYCHAIN_FAILURE = 3
};

static const size_t maximum_credential_bytes = 1024 * 1024;

static CFMutableDictionaryRef credential_query(const uint8_t *identifier, size_t length) {
    if (!identifier || !length || length > 4096 || memchr(identifier, 0, length)) return NULL;
    CFStringRef account = CFStringCreateWithBytes(kCFAllocatorDefault,
        identifier, (CFIndex)length, kCFStringEncodingUTF8, false);
    if (!account) return NULL;
    CFMutableDictionaryRef query = CFDictionaryCreateMutable(kCFAllocatorDefault,
        0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
    if (query) {
        CFDictionarySetValue(query, kSecClass, kSecClassGenericPassword);
        CFDictionarySetValue(query, kSecAttrService,
            CFSTR("com.digitallyinduced.haskell-agent.mcp-oauth"));
        CFDictionarySetValue(query, kSecAttrAccount, account);
        CFDictionarySetValue(query, kSecAttrSynchronizable, kCFBooleanFalse);
    }
    CFRelease(account);
    return query;
}

/* 0 copied; 1 absent; 2 invalid input; 3 platform failure. No OS error text
 * escapes, because callers must not accidentally log protected record details.
 */
int32_t agent_mcp_keychain_read(
    const uint8_t *identifier, size_t identifier_length,
    uint8_t **output, size_t *output_length
) {
    if (output) *output = NULL;
    if (output_length) *output_length = 0;
    if (!output || !output_length) return MCP_KEYCHAIN_INVALID_INPUT;
    CFMutableDictionaryRef query = credential_query(identifier, identifier_length);
    if (!query) return MCP_KEYCHAIN_INVALID_INPUT;
    CFDictionarySetValue(query, kSecReturnData, kCFBooleanTrue);
    CFDictionarySetValue(query, kSecMatchLimit, kSecMatchLimitOne);
    CFTypeRef result = NULL;
    OSStatus status = SecItemCopyMatching(query, &result);
    CFRelease(query);
    if (status == errSecItemNotFound) return MCP_KEYCHAIN_NOT_FOUND;
    if (status != errSecSuccess || !result) {
        if (result) CFRelease(result);
        return MCP_KEYCHAIN_FAILURE;
    }
    if (CFGetTypeID(result) != CFDataGetTypeID()) {
        CFRelease(result);
        return MCP_KEYCHAIN_FAILURE;
    }
    CFDataRef data = (CFDataRef)result;
    CFIndex length = CFDataGetLength(data);
    if (length <= 0 || (uint64_t)length > maximum_credential_bytes) {
        CFRelease(result);
        return MCP_KEYCHAIN_FAILURE;
    }
    uint8_t *copy = malloc((size_t)length);
    if (!copy) {
        CFRelease(result);
        return MCP_KEYCHAIN_FAILURE;
    }
    memcpy(copy, CFDataGetBytePtr(data), (size_t)length);
    CFRelease(result);
    *output = copy;
    *output_length = (size_t)length;
    return MCP_KEYCHAIN_SUCCESS;
}

int32_t agent_mcp_keychain_write(
    const uint8_t *identifier, size_t identifier_length,
    const uint8_t *value, size_t value_length
) {
    if (!value || !value_length || value_length > maximum_credential_bytes)
        return MCP_KEYCHAIN_INVALID_INPUT;
    CFMutableDictionaryRef query = credential_query(identifier, identifier_length);
    if (!query) return MCP_KEYCHAIN_INVALID_INPUT;
    CFDataRef data = CFDataCreate(kCFAllocatorDefault, value, (CFIndex)value_length);
    if (!data) {
        CFRelease(query);
        return MCP_KEYCHAIN_FAILURE;
    }
    const void *keys[] = {kSecValueData};
    const void *values[] = {data};
    CFDictionaryRef attributes = CFDictionaryCreate(kCFAllocatorDefault,
        keys, values, 1, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
    if (!attributes) {
        CFRelease(data);
        CFRelease(query);
        return MCP_KEYCHAIN_FAILURE;
    }
    OSStatus status = SecItemUpdate(query, attributes);
    if (status == errSecItemNotFound) {
        CFDictionarySetValue(query, kSecValueData, data);
        CFDictionarySetValue(query, kSecAttrLabel, CFSTR("Haskell Agent MCP authorization"));
        status = SecItemAdd(query, NULL);
        /* Another process may create this account between update and add. */
        if (status == errSecDuplicateItem) {
            CFDictionaryRemoveValue(query, kSecValueData);
            CFDictionaryRemoveValue(query, kSecAttrLabel);
            status = SecItemUpdate(query, attributes);
        }
    }
    CFRelease(attributes);
    CFRelease(data);
    CFRelease(query);
    return status == errSecSuccess ? MCP_KEYCHAIN_SUCCESS : MCP_KEYCHAIN_FAILURE;
}

int32_t agent_mcp_keychain_delete(const uint8_t *identifier, size_t identifier_length) {
    CFMutableDictionaryRef query = credential_query(identifier, identifier_length);
    if (!query) return MCP_KEYCHAIN_INVALID_INPUT;
    OSStatus status = SecItemDelete(query);
    CFRelease(query);
    return status == errSecSuccess || status == errSecItemNotFound
        ? MCP_KEYCHAIN_SUCCESS : MCP_KEYCHAIN_FAILURE;
}

void agent_mcp_keychain_release(uint8_t *bytes, size_t length) {
    if (bytes) {
        volatile uint8_t *destination = bytes;
        for (size_t index = 0; index < length; index++) destination[index] = 0;
        free(bytes);
    }
}
