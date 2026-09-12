/* Pinned MicroHs FFI wrappers for yyjson. The document owns all node pointers. */
#include "mhsffi.h"
#include <yyjson.h>
#include <string.h>

static yyjson_doc *native_json_parse(const char *source) {
    return yyjson_read(source, strlen(source), YYJSON_READ_NUMBER_AS_RAW);
}

static intptr_t native_json_kind(yyjson_val *value) {
    if (yyjson_is_null(value)) return 0;
    if (yyjson_is_false(value)) return 1;
    if (yyjson_is_true(value)) return 2;
    if (yyjson_is_raw(value)) return 3;
    if (yyjson_is_str(value)) return 4;
    if (yyjson_is_arr(value)) return 5;
    if (yyjson_is_obj(value)) return 6;
    return -1;
}

static const char *native_json_text(yyjson_val *value) {
    return yyjson_is_raw(value) ? yyjson_get_raw(value) : yyjson_get_str(value);
}

static yyjson_val *native_json_child(yyjson_val *value) {
    return value + 1;
}

static yyjson_val *native_json_next(yyjson_val *value) {
    /* yyjson's documented array iterator advances over nested containers. */
    yyjson_arr_iter iterator;
    iterator.idx = 0;
    iterator.max = 1;
    iterator.cur = value;
    yyjson_arr_iter_next(&iterator);
    return iterator.cur;
}

static from_t mhs_native_json_parse(int s) {
    return mhs_from_Ptr(s, 1, native_json_parse(mhs_to_Ptr(s, 0)));
}
static from_t mhs_native_json_free(int s) {
    yyjson_doc_free(mhs_to_Ptr(s, 0));
    return mhs_from_Unit(s, 1);
}
static from_t mhs_native_json_root(int s) {
    return mhs_from_Ptr(s, 1, yyjson_doc_get_root(mhs_to_Ptr(s, 0)));
}
static from_t mhs_native_json_kind(int s) {
    return mhs_from_Int(s, 1, native_json_kind(mhs_to_Ptr(s, 0)));
}
static from_t mhs_native_json_length(int s) {
    return mhs_from_Int(s, 1, yyjson_get_len(mhs_to_Ptr(s, 0)));
}
static from_t mhs_native_json_text(int s) {
    return mhs_from_Ptr(s, 1, (void *)native_json_text(mhs_to_Ptr(s, 0)));
}
static from_t mhs_native_json_child(int s) {
    return mhs_from_Ptr(s, 1, native_json_child(mhs_to_Ptr(s, 0)));
}
static from_t mhs_native_json_next(int s) {
    return mhs_from_Ptr(s, 1, native_json_next(mhs_to_Ptr(s, 0)));
}
