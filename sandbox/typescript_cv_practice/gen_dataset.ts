// ---
// jupyter:
//   jupytext:
//     text_representation:
//       extension: .ts
//       format_name: light
//       format_version: '1.5'
//       jupytext_version: 1.16.4
//   kernelspec:
//     display_name: TypeScript
//     language: typescript
//     name: tslab
// ---

// +
import { imread, imwrite } from './io';
import { render } from './render';
import { warpRect } from './improc';
import { rotate, translate, compose, invert } from './transform';

import path from 'path';
import { globSync } from 'glob';
// -

const addPrefix = (filepath: string, prefix: string) => {
    const lastDotIndex = filepath.lastIndexOf('.');
    if (lastDotIndex === -1) {
        // No extension
        return `${filepath}${prefix}`;
    }
    const name = filepath.slice(0, lastDotIndex);
    const extension = filepath.slice(lastDotIndex);
    return `${name}${prefix}${extension}`;
}

const org_data_dir = "assets/org/";
const org_files = globSync(path.join(org_data_dir, "*.jpg"));

const tr_5_5 = translate(-5, -5)
const rot_256 = 3.14 / 256

for (const file of org_files) {
    const org_img = await imread(file);
    const img_shape = { origin: { x: 0, y: 0 }, width: org_img.width, height: org_img.height }

    const to_center = translate(org_img.width / 2, org_img.height / 2)
    const tr_rot = compose(to_center, compose(rotate(rot_256), invert(to_center)))

    const tr_img = render(org_img, [warpRect(img_shape, tr_5_5)])
    const tr_img_file = addPrefix(file, "_tr_5_5")
    await imwrite(tr_img_file, tr_img)

    const rot_img = render(org_img, [warpRect(img_shape, tr_rot)])
    const rot_img_file = addPrefix(file, "_rot_256")
    await imwrite(rot_img_file, rot_img)
}

