import { scanRect } from './sampling'
import { Transform, apply as applyTransform } from './transform'
import { Image, imcopy } from './image'
import { Rect, contain } from './geometry'
import { Command } from './render'

const warpRect = (rect: Rect, t: Transform): Command => {
    return {
        apply: (buf): Image => {
            const org_buf = imcopy(buf)
            const buf_region = {
                origin: { x: 0, y: 0 },
                width: buf.width,
                height: buf.height
            }

            scanRect(rect, (src) => {
                const src_x = Math.floor(src.x)
                const src_y = Math.floor(src.y)
                const src_idx = 4 * (src_y * buf.width + src_x)
                if (!contain(buf_region, src)) {
                    buf.data[src_idx] = 0xff
                    buf.data[src_idx + 1] = 0
                    buf.data[src_idx + 2] = 0
                    return
                }

                const dst = applyTransform(t, src)
                const dst_x = Math.floor(dst.x)
                const dst_y = Math.floor(dst.y)
                const dst_idx = 4 * (dst_y * buf.width + dst_x)
                if (!contain(buf_region, dst)) {
                    buf.data[src_idx] = 0xff
                    buf.data[src_idx + 1] = 0xff
                    buf.data[src_idx + 2] = 0xff
                    return
                }

                buf.data[src_idx] = org_buf.data[dst_idx]
                buf.data[src_idx + 1] = org_buf.data[dst_idx + 1]
                buf.data[src_idx + 2] = org_buf.data[dst_idx + 2]
            })

            return buf
        }
    }
}

export {
    warpRect
}