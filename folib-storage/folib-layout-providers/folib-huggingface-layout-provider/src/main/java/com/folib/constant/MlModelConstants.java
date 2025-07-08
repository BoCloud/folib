package com.folib.constant;

public final class MlModelConstants {
    public static final String X_REPO_COMMIT_HEADER = "X-Repo-Commit";

    public static final String X_ERROR_CODE_HEADER = "X-Error-Code";

    public static final String X_ERROR_MESSAGE_HEADER = "X-Error-Message";

    public static final String X_LINKED_ETAG = "X-Linked-ETag";

    public static final String X_LINKED_SIZE = "X-Linked-Size";

    public static final String ENTITY_NOT_FOUND_HEADER_VALUE = "EntryNotFound";

    public static final String CACHE_FOLDER = ".folib";

    public static final String UPLOADS_FOLDER = "_uploads";

    public static final String LAST_UPDATED_PROP_KEY = "hf_last_updated";

    public static final String LEAD_FILE_NAME = ".folib_huggingface_model_info.json";

    public static final String LATEST_LEAD_FILE_NAME = ".latest_huggingface_model_info.json";

    public static final String HF_ID = "huggingfaceml.id";

    public static final String HF_VERSION = "huggingfaceml.version";

    public static final String HF_GENERATED_REVISION_SHA1 = "huggingfaceml.generated.revision.sha1";

    public static final String HF_AUTHOR = "huggingfaceml.author";

    public static final String HF_LAST_MODIFIED = "huggingfaceml.lastModified";

    public static final String HF_LIBRARY_NAME = "huggingfaceml.libraryName";

    public static final String HF_TAGS = "huggingfaceml.tags";

    public static final String HF_LANGUAGE = "huggingfaceml.lang";

    public static final String HF_LICENSE = "huggingfaceml.license";

    public static final String HF_ETAG_FILE = "huggingfaceml.etag.file";

    public static final String ML_HANDLE_COMMIT_GUARD = "handle_commit_guard";


    private MlModelConstants() {
        throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
    }
}
