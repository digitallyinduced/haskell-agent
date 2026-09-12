"""Optional native decoder checks; set MICROHS_NATIVE_INTERPRETER to enable."""

import json
import os
import unittest

from experiment import temporary_directory
from persistent import PersistentMicroHs


@unittest.skipUnless(os.environ.get("MICROHS_NATIVE_INTERPRETER"), "native interpreter not configured")
class NativeDecoderTests(unittest.IsolatedAsyncioTestCase):
    async def test_native_decoder_and_tool_round_trip(self):
        with temporary_directory() as directory:
            async with PersistentMicroHs(
                    directory, interpreter=os.environ["MICROHS_NATIVE_INTERPRETER"],
                    modules=("CodeMode.NativeJson",)) as worker:
                documents = ["null", "true", "false", "-0", "1.23e+400",
                             '"Grüße λ 😀"', '"\\u0000\\n\\t"',
                             '{"nested":[1,null,true,{"x":[]}]}',
                             "123456789012345678901234567890.123456789e+123",
                             "[" * 127 + "0" + "]" * 127]
                # JSON string escaping is a Haskell-compatible literal here for ASCII
                # input; direct Unicode is retained rather than using JSON's \\u syntax.
                for document in documents:
                    literal = json.dumps(document, ensure_ascii=False)
                    result = await worker.execute(
                        f'do {{ actual <- decodeJsonNative {literal}; '
                        f'emit (JsonBool (actual == decodeJson {literal})) }}')
                    self.assertEqual(result["content"], [True], result)
                invalid = ["", "01", "+1", "1.", "1e+", "NaN", "Infinity", "[1,]",
                           '{"a":1,"a":2}', '{"a":1,"\\u0061":2}',
                           '"\\uD800"', '"\\uDC00"', '"\\uD800\\u0041"',
                           "null false", "[" * 128 + "0" + "]" * 128]
                for document in invalid:
                    result = await worker.execute(
                        f'do {{ actual <- decodeJsonNative {json.dumps(document)}; '
                        'emit (JsonBool (case actual of { Left _ -> True; Right _ -> False })) }')
                    self.assertEqual(result["content"], [True], result)
                value = {"unicode": "Grüße λ 😀", "control": "\x00\b\f\n\r\t\x1f",
                         "values": [None, True, False, -50.25, 10**40, {"nested": []}]}
                result = await worker.execute(
                    'do { first <- callToolWithDecoder decodeJsonNative "fixture.echo" JsonNull; '
                    'second <- callToolWithDecoder decodeJsonNative "fixture.echo" first; emit second }',
                    handler=lambda name, arguments: value if arguments is None else arguments)
                self.assertEqual(result["content"], [value], result)
                self.assertEqual(result["calls"], 2)
                denied = await worker.execute(
                    'callToolWithDecoder decodeJsonNative "fixture.denied" JsonNull >>= emit')
                self.assertIn("approval denied", denied["completion"]["error"]["message"])
                recovery = await worker.execute('emit (JsonBool True)')
                self.assertEqual(recovery["content"], [True])
