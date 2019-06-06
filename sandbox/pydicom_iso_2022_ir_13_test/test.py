import pydicom

japanese_pn = u"ﾔﾏﾀﾞ^ﾀﾛｳ=山田^太郎=やまだ^たろう"
specific_character_sets = ["ISO 2022 IR 13", "ISO 2022 IR 87"]
expect_encoded = (
    b"\xd4\xcf\xc0\xde\x5e\xc0\xdb\xb3\x3d\x1b\x24\x42\x3b\x33"
    b"\x45\x44\x1b\x28\x4a\x5e\x1b\x24\x42\x42\x40\x4f\x3a\x1b"
    b"\x28\x4a\x3d\x1b\x24\x42\x24\x64\x24\x5e\x24\x40\x1b\x28"
    b"\x4a\x5e\x1b\x24\x42\x24\x3f\x24\x6d\x24\x26\x1b\x28\x4a"
)

python_encodings = pydicom.charset.convert_encodings(specific_character_sets)
actual_encoded = pydicom.charset.encode_string(japanese_pn, python_encodings)

print("actual:{}".format(actual_encoded))
print("expect:{}".format(expect_encoded))
