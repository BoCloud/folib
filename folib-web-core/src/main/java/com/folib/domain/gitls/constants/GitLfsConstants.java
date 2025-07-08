package com.folib.domain.gitls.constants;

public interface GitLfsConstants {
    public static final String LFS_API = "lfs";

    public static final String GITLFS_ENDPOINT = "gitlfs";

    public static final String OID = "OID";

    public static final String OBJECTS = "objects";

    public static final String LOCKS_DIR = ".jfrog/locks";

    public static final String PROP_LOCK_OWNER = "lock.owner";

    public static final String SHA_256_HEADER = "X-Checksum-Sha256";

    public static final String LFS_JSON = "application/vnd.git-lfs+json";

    public static final String BATCH_OPERATION_DOWNLOAD = "download";

    public static final String BATCH_OPERATION_UPLOAD = "upload";

    public static final String NOT_FOUND_MESSAGE = "Not Found";

    public static final String NO_DEPLOY_MESSAGE = "You do not have deploy permissions for the requested path";

    public static final String NO_READ_MESSAGE = "You do not have read permissions for the requested path";

    public static final String UNSUPPORTED_OPERATION = "Unsupported operation";

    public static final String PATH_SEPARATOR = "/";
}
