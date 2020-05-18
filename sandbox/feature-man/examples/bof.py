import feature_man

paths = ["../tests/data/img_000.jpg", "../tests/data/img_001.jpg"]

scheduler = feature_man.scheduler.ParallelScheduler(
    [
        feature_man.scheduler.MapTask(
            feature_man.ops.container.ChainContainer(
                [
                    feature_man.ops.container.Node(
                        feature_man.ops.mapper.GrayscaleMapper(), "grayscale",
                    ),
                    feature_man.ops.container.Node(
                        feature_man.ops.detector.GridDetector(12, 12, 20), "det",
                    ),
                    feature_man.ops.container.Node(
                        feature_man.ops.descriptor.KAZEDescriptor(),
                        source=("grayscale", "det",),
                    ),
                ]
            ),
        ),
        feature_man.scheduler.ReduceTask(
            feature_man.ops.container.TeeContainer(
                feature_man.ops.container.ChainContainer(
                    [
                        feature_man.ops.Concat(0),
                        feature_man.ops.KMeansClustering(4),
                        feature_man.ops.PickleExport("codebook.pckl"),
                    ]
                )
            )
        ),
        feature_man.scheduler.MapTask(
            feature_man.ops.container.ChainContainer(
                [
                    feature_man.ops.BruteForceQuanize("codebook.pckl"),
                    feature_man.ops.Histogram(4),
                ]
            )
        )
    ]
)

res = scheduler(paths)
print(res)
