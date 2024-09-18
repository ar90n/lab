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
import { imread, imshow, imwrite } from './io';

import path from 'path';
import { globSync } from 'glob';
import { isObject } from 'util';

import { Circle, Line, intersection } from './geometry'
import { Color } from './color'
import { Command } from './render'
import { drawCircle, drawLine } from './draw';
import { render } from './render';
// -
const org_data_dir = "assets/org/";
const org_files = globSync(path.join(org_data_dir, "*_rot_256.jpg"));

const file = org_files[0];
const org_img = await imread(file);

// -
imshow(org_img);

// -
const cx = org_img.width / 2;
const cy = org_img.height / 2;

// -
const stride = 8;
const beg_y = Math.floor(cy) - 200;
const end_y = Math.floor(cy) + 200;
const beg_x = Math.floor(cx) - 200;
const end_x = Math.floor(cx) + 200;

const left_xs = [];
const left_ys = [];
for (let y = beg_y; y < end_y; y += stride) {
    for (let x = 0; x < cx; x += 1) {
        const ind = 4 * (y * org_img.width + x);
        const val = org_img.data[ind];
        if (val < 128) {
            left_xs.push(x);
            left_ys.push(y);
            break;
        }
    }
}

const right_xs = [];
const right_ys = [];
for (let y = beg_y; y < end_y; y += stride) {
    for (let x = org_img.width - 1; cx < x; x -= 1) {
        const ind = 4 * (y * org_img.width + x);
        const val = org_img.data[ind];
        if (val < 128) {
            right_xs.push(x);
            right_ys.push(y);
            break;
        }
    }
}

// const right_xs = [];
// const right_ys = [];
// for (let y = beg; y < end; y += stride) {
//     for (let x = 0; x < cx; x += 1) {
//         const ind = 4 * (y * org_img.width + x);
//         const val = org_img.data[ind];
//         if (val < 128) {
//             left_xs.push(x);
//             left_ys.push(y);
//             break;
//         }
//     }
// }

const top_xs = [];
const top_ys = [];
for (let x = beg_x; x < end_x; x += stride) {
    for (let y = 0; y < cy; y += 1) {
        const ind = 4 * (y * org_img.width + x);
        const val = org_img.data[ind];
        if (val < 128) {
            top_xs.push(x);
            top_ys.push(y);
            break;
        }
    }
}

const bottom_xs = [];
const bottom_ys = [];
for (let x = beg_x; x < end_x; x += stride) {
    for (let y = org_img.height - 1; cy < y; y -= 1) {
        const ind = 4 * (y * org_img.width + x);
        const val = org_img.data[ind];
        if (val < 128) {
            bottom_xs.push(x);
            bottom_ys.push(y);
            break;
        }
    }
}

let mean_left_x = 0;
let mean_left_y = 0;
for (let i = 0; i < left_xs.length; i++) {
    mean_left_x += left_xs[i];
    mean_left_y += left_ys[i];
}
mean_left_x /= left_xs.length;
mean_left_y /= left_ys.length;

let mean_right_x = 0;
let mean_right_y = 0;
for (let i = 0; i < right_xs.length; i++) {
    mean_right_x += right_xs[i];
    mean_right_y += right_ys[i];
}
mean_right_x /= right_xs.length;
mean_right_y /= right_ys.length;


let mean_top_x = 0;
let mean_top_y = 0;
for (let i = 0; i < top_xs.length; i++) {
    mean_top_x += top_xs[i];
    mean_top_y += top_ys[i];
}
mean_top_x /= top_xs.length;
mean_top_y /= top_ys.length;

let mean_bottom_x = 0;
let mean_bottom_y = 0;
for (let i = 0; i < bottom_xs.length; i++) {
    mean_bottom_x += bottom_xs[i];
    mean_bottom_y += bottom_ys[i];
}
mean_bottom_x /= bottom_xs.length;
mean_bottom_y /= bottom_ys.length;

const centerized_left_xs = left_xs.map(x => x - mean_left_x);
const centerized_left_ys = left_ys.map(y => y - mean_left_y);
const centerized_right_xs = right_xs.map(x => x - mean_right_x);
const centerized_right_ys = right_ys.map(y => y - mean_right_y);
const centerized_top_xs = top_xs.map(x => x - mean_top_x);
const centerized_top_ys = top_ys.map(y => y - mean_top_y);
const centerized_bottom_xs = bottom_xs.map(x => x - mean_bottom_x);
const centerized_bottom_ys = bottom_ys.map(y => y - mean_bottom_y);

// +
let cov = 0;
let n = 0;
for (let i = 0; i < centerized_left_xs.length; i++) {
    cov += centerized_left_xs[i] * centerized_left_ys[i];
}
n += centerized_left_xs.length;
for (let i = 0; i < centerized_right_xs.length; i++) {
    cov += centerized_right_xs[i] * centerized_right_ys[i];
}
n += centerized_right_xs.length;
for (let i = 0; i < centerized_top_xs.length; i++) {
    cov += -centerized_top_xs[i] * centerized_top_ys[i];
}
n += centerized_top_xs.length;
for (let i = 0; i < centerized_bottom_xs.length; i++) {
    cov += -centerized_bottom_xs[i] * centerized_bottom_ys[i];
}
n += centerized_bottom_xs.length;
cov /= n

let var_x = 0;
n = 0
for (let i = 0; i < centerized_left_xs.length; i++) {
    var_x += centerized_left_xs[i] * centerized_left_xs[i];
}
n += centerized_left_xs.length;
for (let i = 0; i < centerized_right_xs.length; i++) {
    var_x += centerized_right_xs[i] * centerized_right_xs[i];
}
n += centerized_right_xs.length;
for (let i = 0; i < centerized_top_ys.length; i++) {
    var_x += centerized_top_ys[i] * centerized_top_ys[i];
}
n += centerized_top_ys.length;
for (let i = 0; i < centerized_bottom_ys.length; i++) {
    var_x += centerized_bottom_ys[i] * centerized_bottom_ys[i];
}
var_x /= n

let var_y = 0;
n = 0
for (let i = 0; i < centerized_left_ys.length; i++) {
    var_y += centerized_left_ys[i] * centerized_left_ys[i];
}
n += centerized_left_ys.length;
for (let i = 0; i < centerized_right_ys.length; i++) {
    var_y += centerized_right_ys[i] * centerized_right_ys[i];
}
n += centerized_right_ys.length;
for (let i = 0; i < centerized_top_xs.length; i++) {
    var_y += centerized_top_xs[i] * centerized_top_xs[i];
}
n += centerized_top_xs.length;
for (let i = 0; i < centerized_bottom_xs.length; i++) {
    var_y += centerized_bottom_xs[i] * centerized_bottom_xs[i];
}
n += centerized_bottom_xs.length;
var_y /= n
// -

const diff_var = var_x - var_y;
const delta = diff_var * diff_var + 4 * cov * cov;
const a = 2 * cov;
const b = var_y - var_x + Math.sqrt(delta);
const norm_a = a / Math.sqrt(a * a + b * b);
const norm_b = b / Math.sqrt(a * a + b * b);
const nx = -norm_b
const ny = norm_a;

const left_c = -nx * mean_left_x - ny * mean_left_y;
const left_line = { nx:nx, ny:ny, c:left_c };
const right_c = -nx * mean_right_x - ny * mean_right_y;
const right_line = { nx:nx, ny:ny, c:right_c };
const top_c = -ny * mean_top_x + nx * mean_top_y;
const top_line = { nx:ny, ny:-nx, c:top_c };
const bottom_c = -ny * mean_bottom_x + nx * mean_bottom_y;
const bottom_line = { nx:ny, ny:-nx, c:bottom_c };

const raw_left_top = intersection(left_line, top_line);
const left_top = { x: Math.floor(raw_left_top.x), y: Math.floor(raw_left_top.y) };
const raw_left_bottom = intersection(left_line, bottom_line);
const left_bottom = { x: Math.floor(raw_left_bottom.x), y: Math.floor(raw_left_bottom.y) };
const raw_right_top = intersection(right_line, top_line);
const right_top = { x: Math.floor(raw_right_top.x), y: Math.floor(raw_right_top.y) };
const raw_right_bottom = intersection(right_line, bottom_line);
const right_bottom = { x: Math.floor(raw_right_bottom.x), y: Math.floor(raw_right_bottom.y) };

let commands = []
commands.push(drawCircle({center: left_top, radius:8}, Color.red));
commands.push(drawCircle({center: right_top, radius:8}, Color.red));
commands.push(drawCircle({center: left_bottom, radius:8}, Color.red));
commands.push(drawCircle({center: right_bottom, radius:8}, Color.red));
commands.push(drawLine(left_line, Color.red));
commands.push(drawLine(right_line, Color.red));
commands.push(drawLine(top_line, Color.red));
commands.push(drawLine(bottom_line, Color.red));

await imshow(render(org_img, commands));


