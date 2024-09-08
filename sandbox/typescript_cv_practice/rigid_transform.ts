class Point {
    constructor(public x: number, public y: number) { }
}

class HomogeneousPoint {
    constructor(public x: number, public y: number, public z: number, public w: number = 1) { }

    toCartesian(): Point {
        if (this.w === 0) {
            throw new Error("Cannot convert to Cartesian coordinates: w is zero");
        }
        return new Point(this.x / this.w, this.y / this.w);
    }

    static fromCartesian(point: Point): HomogeneousPoint {
        return new HomogeneousPoint(point.x, point.y, 0, 1);
    }
}

class RigidTransform2D {
    private matrix: number[][];

    constructor() {
        this.matrix = [
            [1, 0, 0],
            [0, 1, 0],
            [0, 0, 1]
        ];
    }

    rotate(angle: number): RigidTransform2D {
        const cos = Math.cos(angle);
        const sin = Math.sin(angle);
        const rotationMatrix = [
            [cos, -sin, 0],
            [sin, cos, 0],
            [0, 0, 1]
        ];
        this.matrix = this.multiplyMatrices(rotationMatrix, this.matrix);
        return this;
    }

    translate(dx: number, dy: number): RigidTransform2D {
        const translationMatrix = [
            [1, 0, dx],
            [0, 1, dy],
            [0, 0, 1]
        ];
        this.matrix = this.multiplyMatrices(translationMatrix, this.matrix);
        return this;
    }

    applyToPoint(point: Point): Point {
        const [x, y] = this.multiplyMatrixVector(this.matrix, [point.x, point.y, 1]);
        return new Point(x, y);
    }

    // New method to get the transformation matrix in the format Canvas API expects
    getCanvasTransform(): [number, number, number, number, number, number] {
        const [[a, b, e], [c, d, f]] = this.matrix;
        return [a, b, c, d, e, f];
    }

    private multiplyMatrices(a: number[][], b: number[][]): number[][] {
        return a.map((row, i) =>
            b[0].map((_, j) =>
                row.reduce((sum, elm, k) => sum + elm * b[k][j], 0)
            )
        );
    }

    private multiplyMatrixVector(matrix: number[][], vector: number[]): number[] {
        return matrix.map(row =>
            row.reduce((sum, value, index) => sum + value * vector[index], 0)
        );
    }
}

// Function to apply RigidTransform2D to a canvas context
function applyTransformToCanvas(ctx: CanvasRenderingContext2D, transform: RigidTransform2D) {
    const [a, b, c, d, e, f] = transform.getCanvasTransform();
    ctx.transform(a, b, c, d, e, f);
}