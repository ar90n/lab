// ---
// jupyter:
//   jupytext:
//     text_representation:
//       extension: .ts
//       format_name: percent
//       format_version: '1.3'
//       jupytext_version: 1.16.4
//   kernelspec:
//     display_name: TypeScript
//     language: typescript
//     name: tslab
// ---

// %%
import {imshow, imload} from './util';

// %%
const img = await imload("meerkat.jpg");

// %%
await imshow(img);
