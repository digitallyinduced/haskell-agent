# Map results, version 1

`display_map` accepts a title and 1–100 locations and returns a persistent
presentation document as its successful tool output. It has no external side
effects. The tool does not geocode, search, request location permission, or
verify coordinates. Supply coordinates from the user or retrieved sources;
do not invent precise coordinates.

```json
{
  "type": "map",
  "version": 1,
  "title": "Berlin",
  "locations": [
    {
      "id": "brandenburg-gate",
      "name": "Brandenburg Gate",
      "latitude": 52.516275,
      "longitude": 13.377704,
      "address": null,
      "description": null
    }
  ],
  "text": "Berlin\nLocations use supplied coordinates; they have not been verified by a map service.\n1. Brandenburg Gate (52.516275, 13.377704)"
}
```

The arguments contain only `title` and `locations`; the runtime adds the
discriminator, version, and fallback text. Location IDs are unique within a
result and remain stable on reload. Follow-up maps create new tool results.

Limits are measured in UTF-8 bytes: title 200, ID 64, name 200, address 500,
description 1000. Supplied strings must be nonblank. Optional address and
description may be omitted or null. C0 controls other than tab and newline
are rejected. Coordinates are finite JSON numbers within inclusive latitude
[-90, 90] and longitude [-180, 180]. Duplicate coordinates are permitted.
The complete serialized output, including fallback text, is at most 256 KiB.
Inputs exceeding limits fail atomically rather than producing a partial map.
Consumers ignore additional fields but must reject unknown versions, invalid
coordinates, duplicate IDs, and incomplete/truncated documents.

The document is an intentionally versioned cross-client presentation protocol.
It travels in the existing persisted function-tool output and versioned native
loop event output field. No new in-process JSON request API is introduced.
Validated documents retain their complete content through native event framing,
session JSON hydration, and oversized-output finalization. Clients must
preserve them when compacting display rows, rather than truncating arguments
and attempting to reconstruct a map from those arguments.

Renderers recognize maps only for successful `display_map` calls, not arbitrary
JSON in other tool output or assistant prose. Native map renderers should keep
results visible outside collapsed technical-work groups. Other clients can
render `text` or derive an equivalent numbered list from validated locations.
Text and coordinates remain useful without map tiles. Unknown versions and
corrupt historical output fall back to ordinary textual tool presentation.

Map tile requests are made by the native map component to its map provider.
Do not claim coordinates have been verified by that provider. Launching an
external maps application requires an explicit user action.
