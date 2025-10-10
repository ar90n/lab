#!/usr/bin/env python3
import argparse, json, csv, sys, re
from collections import defaultdict

# ヘルパ：dictツリーを走査し、キー名にpatternを含む値を拾う（大文字小文字無視）
def deep_get(d, key_patterns):
    if isinstance(d, dict):
        for k, v in d.items():
            if any(p in k.lower() for p in key_patterns):
                return v
            got = deep_get(v, key_patterns)
            if got is not None:
                return got
    elif isinstance(d, list):
        for it in d:
            got = deep_get(it, key_patterns)
            if got is not None:
                return got
    return None

def ensure_list(x):
    if x is None: return []
    if isinstance(x, list): return x
    # "R1,R2 R3" 的な文字列も分解
    if isinstance(x, str):
        s = re.sub(r"[;\s]+", ",", x.strip())
        return [t for t in s.split(",") if t]
    return [str(x)]

def first_nonempty(*vals):
    for v in vals:
        if v:
            if isinstance(v, str) and v.strip():
                return v.strip()
            if not isinstance(v, str):
                return v
    return ""

def normalize_lcsc(s):
    if not s: return ""
    s = s.strip().upper()
    if s.startswith("C") and re.fullmatch(r"C\d+", s):
        return s
    # "LCSC:C12345" 等も正規化
    m = re.search(r"C\d+", s)
    return m.group(0) if m else s

def main():
    ap = argparse.ArgumentParser(description="Convert JSON BOM to JLCPCB BOM CSV")
    ap.add_argument("json_path")
    ap.add_argument("-o","--output", default="jlcpcb_bom.csv")
    args = ap.parse_args()

    with open(args.json_path, "r", encoding="utf-8") as f:
        data = json.load(f)

    # ルートが配列 or {items:[...]} / {components:[...]} などを吸収
    if isinstance(data, dict):
        for k in ["items","components","parts","bom"]:
            if k in data and isinstance(data[k], list):
                data = data[k]
                break
        if isinstance(data, dict):
            print("ERROR: JSONの最上位に配列/parts一覧が見つかりませんでした。", file=sys.stderr)
            sys.exit(1)
    elif not isinstance(data, list):
        print("ERROR: 未対応のJSON構造です。", file=sys.stderr)
        sys.exit(1)

    # まとめキー：(Comment, Footprint, LCSC) ごとにDesignatorをまとめて1行化
    grouped = defaultdict(lambda: {"refs":[],"qty":0})
    for item in data:
        # designators
        refs = ensure_list(first_nonempty(
            item.get("designators"),
            item.get("designator"),
            item.get("reference"),
            item.get("references"),
            item.get("refdes"),
            deep_get(item, ["designators","designator","reference","references","refdes","refs"])
        ))

        # comment/value/description
        comment = first_nonempty(
            item.get("value"),
            item.get("comment"),
            item.get("description"),
            item.get("name"),
            item.get("label"),
            deep_get(item, ["value","comment","description","name","label"])
        )

        # footprint/package
        footprint = first_nonempty(
            item.get("footprint"),
            item.get("package"),
            item.get("pkg"),
            deep_get(item, ["footprint","package","pkg"])
        )

        # LCSC Part #
        lcsc = normalize_lcsc(first_nonempty(
            item.get("lcsc"),
            item.get("lcsc_part"),
            item.get("lcsc#"),
            item.get("lcsc_number"),
            item.get("jlcpcb"),
            item.get("jlcpcb_part"),
            deep_get(item, ["lcsc","jlcpcb","lcsc_part","lcsc#","lcsc_number","jlcpcb_part"])
        ))

        # MPN（補助：BOMでMPNを見たいときに備えて拾っておく）
        mpn = first_nonempty(
            item.get("mpn"),
            item.get("mfr_part"),
            item.get("manufacturer_part_number"),
            deep_get(item, ["mpn","mfr_part","manufacturer_part_number"])
        )

        # コメントが空ならMPNで補完、さらに空ならフットプリントで代用
        if not comment:
            comment = first_nonempty(mpn, footprint, "N/A")

        key = (comment, footprint, lcsc, mpn)
        grouped[key]["refs"].extend(refs)
        grouped[key]["qty"] += max(len(refs), int(item.get("qty", 0)) if str(item.get("qty","")).isdigit() else 0)

    # CSV出力（JLCが認識しやすい見出し）
    # 参考：Designator / Comment / Footprint / LCSC Part #（必要最小限）:contentReference[oaicite:1]{index=1}
    with open(args.output, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["Comment","Designator","Footprint","LCSC Part #","MPN","Quantity"])
        for (comment, footprint, lcsc, mpn), info in grouped.items():
            refs = sorted(set(info["refs"]), key=lambda r: (re.sub(r"\D","",r), r))
            qty = len(refs) if info["qty"] == 0 else info["qty"]
            w.writerow([
                comment,
                ",".join(refs),
                footprint,
                lcsc,
                mpn,
                qty
            ])

    print(f"OK: wrote {args.output}")

if __name__ == "__main__":
    main()

