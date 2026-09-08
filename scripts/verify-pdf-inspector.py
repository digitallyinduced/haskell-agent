"""Exercise the installed PDF commands using generated, non-customer data."""

import json
import os
from pathlib import Path
import subprocess
import tempfile


def create_invoice(path, image_only=False):
    content = (
        b"BT /F1 18 Tf 50 760 Td (Rechnung TEST-2026-001) Tj "
        b"/F1 12 Tf 0 -40 Td (Beratung 100,00 EUR) Tj "
        b"0 -25 Td (Umsatzsteuer 19,00 EUR) Tj "
        b"0 -25 Td (Gesamtbetrag 119,00 EUR) Tj ET\n"
    )
    if image_only:
        content = b"q 500 0 0 700 50 50 cm /Im1 Do Q\n"
    objects = [
        b"<< /Type /Catalog /Pages 2 0 R >>",
        b"<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
        b"<< /Type /Page /Parent 2 0 R /MediaBox [0 0 595 842] "
        b"/Resources << /Font << /F1 4 0 R >> "
        + (b"/XObject << /Im1 6 0 R >> " if image_only else b"")
        + b">> /Contents 5 0 R >>",
        b"<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>",
        b"<< /Length " + str(len(content)).encode() + b" >>\nstream\n"
        + content + b"endstream",
        b"<< /Type /XObject /Subtype /Image /Width 1 /Height 1 "
        b"/ColorSpace /DeviceGray /BitsPerComponent 8 /Length 1 >>\n"
        b"stream\n\x80\nendstream",
    ]
    data = bytearray(b"%PDF-1.4\n")
    offsets = []
    for number, obj in enumerate(objects, 1):
        offsets.append(len(data))
        data.extend(f"{number} 0 obj\n".encode() + obj + b"\nendobj\n")
    xref = len(data)
    data.extend(b"xref\n0 7\n0000000000 65535 f \n")
    for offset in offsets:
        data.extend(f"{offset:010} 00000 n \n".encode())
    data.extend(
        f"trailer\n<< /Size 7 /Root 1 0 R >>\nstartxref\n{xref}\n%%EOF\n".encode()
    )
    path.write_bytes(data)


def invoke(*arguments):
    return subprocess.run(
        arguments, check=True, capture_output=True, text=True, timeout=30
    ).stdout


with tempfile.TemporaryDirectory(prefix="pdf-inspector-", dir=os.environ["TMPDIR"]) as directory:
    invoice = Path(directory) / "invoice.pdf"
    create_invoice(invoice)
    original = invoice.read_bytes()
    detection = json.loads(invoke("detect-pdf", str(invoice), "--json"))
    assert detection["pdf_type"] == "text_based", detection
    assert detection["page_count"] == 1, detection
    assert detection["pages_needing_ocr"] == [], detection
    markdown = invoke("pdf2md", str(invoice), "--raw", "--pages")
    expected = [
        "Rechnung TEST-2026-001",
        "Beratung 100,00 EUR",
        "Umsatzsteuer 19,00 EUR",
        "Gesamtbetrag 119,00 EUR",
    ]
    positions = [markdown.index(text) for text in expected]
    assert positions == sorted(positions), markdown
    assert "<!-- Page 1 -->" in markdown, markdown
    result = json.loads(invoke("pdf2md", str(invoice), "--json"))
    assert result["pdf_type"] == "text_based", result
    assert "119,00 EUR" in result["markdown"], result
    assert invoice.read_bytes() == original, "Source document was modified"
    scan = Path(directory) / "image-only.pdf"
    create_invoice(scan, image_only=True)
    scan_detection = json.loads(invoke("detect-pdf", str(scan), "--json"))
    assert scan_detection["ocr_recommended"], scan_detection
    assert scan_detection["pages_needing_ocr"] == [1], scan_detection
    scan_result = json.loads(invoke("pdf2md", str(scan), "--json"))
    assert not scan_result["markdown"], scan_result
    assert scan_result["pages_needing_ocr"] == [1], scan_result
    invalid = Path(directory) / "invalid.pdf"
    invalid.write_text("Not a PDF")
    failure = subprocess.run(
        ["pdf2md", str(invalid), "--raw"], capture_output=True, timeout=30
    )
    assert failure.returncode != 0, "Invalid document was accepted"
print("PDF classification, Markdown, JSON, amounts, reading order, and failure handling passed")
