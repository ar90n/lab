import { intersection, lines_from_rect, line_from_two_points } from './geometry'

describe('geometry', () => {
    describe('intersection', () => {
        it('should return the intersection point of two intersecting lines', () => {
            const l1 = { nx: 1, ny: -1, c: 0 }
            const l2 = { nx: 1, ny: 1, c: 4 }
            const result = intersection(l1, l2)
            expect(result).toEqual({ x: -2, y: -2 })
        })

        it('should throw an error for parallel lines', () => {
            const l1 = { nx: 1, ny: -1, c: 0 }
            const l2 = { nx: 1, ny: -1, c: 4 }
            expect(() => intersection(l1, l2)).toThrow("Lines are parallel")
        })
    })

    describe('line_from_two_points', () => {
        it('should return the correct line equation from two points', () => {
            const p1 = { x: 0, y: 0 }
            const p2 = { x: 4, y: 4 }
            const result = line_from_two_points(p1, p2)
            expect(result.nx).toBeCloseTo(-Math.SQRT1_2)
            expect(result.ny).toBeCloseTo(Math.SQRT1_2)
            expect(result.c).toBe(0)
        })
    })

    describe('lines_from_rect', () => {
        it('should return the correct lines for a rectangle', () => {
            const rect = { origin: { x: -2, y: -1 }, width: 4, height: 2 }
            const result = lines_from_rect(rect)
            expect(result.length).toBe(4)
            expect(result[0].nx).toBeCloseTo(0)
            expect(result[0].ny).toBeCloseTo(1)
            expect(result[0].c).toBeCloseTo(1)
            expect(result[1].nx).toBeCloseTo(-1)
            expect(result[1].ny).toBeCloseTo(0)
            expect(result[1].c).toBeCloseTo(2)
            expect(result[2].nx).toBeCloseTo(0)
            expect(result[2].ny).toBeCloseTo(-1)
            expect(result[2].c).toBeCloseTo(1)
            expect(result[3].nx).toBeCloseTo(1)
            expect(result[3].ny).toBeCloseTo(0)
            expect(result[3].c).toBeCloseTo(2)
        })
    })
})