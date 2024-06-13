package com.veadan.folib.domain.huggingface.constant;

import lombok.Generated;

public final class MlModelSystemProperties {
    @Generated
    private MlModelSystemProperties() {
        throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
    }

    public static final SystemProperty<Integer> ML_MODEL_METADATA_PACKAGES_INDEX_WORKERS = SystemProperty.of("huggingfaceml.metadata.packages.index.workers", Integer.valueOf(5));

    public static final SystemProperty<Integer> ML_MODEL_METADATA_INDEX_WORKERS = SystemProperty.of("huggingfaceml.metadata.calculation.workers", Integer.valueOf(5));

    public static final SystemProperty<Integer> ML_MODEL_CONCURRENT_UPLOADS = SystemProperty.of("huggingfaceml.concurrent.commits.limit", Integer.valueOf(3));

    public static final SystemProperty<Long> ML_MODEL_LFS_FILE_MIN_SIZE = SystemProperty.of("huggingfaceml.lfs.min.size", Long.valueOf(10485760L));
}
