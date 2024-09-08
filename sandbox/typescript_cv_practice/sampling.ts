import { Rect, LineSegment, Point } from './geometry'
import { Transform, apply as applyTransform } from './transform'

const scanRect = (rect: Rect, cb: (src: Point) => void) => {
    const src_x = rect.origin.x
    const src_y = rect.origin.y
    const dst_x = rect.origin.x + rect.width
    const dst_y = rect.origin.y + rect.height
    for (let y = src_y; y < dst_y; y++) {
        for (let x = src_x; x < dst_x; x++) {
            cb({ x, y })
        }
    }
}

const scanLineSegment = (line: LineSegment, cb: (src: Point) => void) => {
    // scan brezenham
    const dx = Math.abs(line.dst.x - line.src.x)
    const dy = Math.abs(line.dst.y - line.src.y)
    const sx = line.src.x < line.dst.x ? 1 : -1
    const sy = line.src.y < line.dst.y ? 1 : -1
    let err = dx - dy
    let x = Math.round(line.src.x)
    let y = Math.round(line.src.y)
    const dst_x = Math.round(line.dst.x)
    const dst_y = Math.round(line.dst.y)
    while (true) {
        if (x === dst_x && y === dst_y) {
            break
        }

        cb({ x, y })

        const e2 = 2 * err
        if (e2 > -dy) {
            err -= dy
            x += sx
        }
        if (e2 < dx) {
            err += dx
            y += sy
        }
    }
}

export {
    scanRect,
    scanLineSegment,
}