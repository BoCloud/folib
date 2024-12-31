package com.veadan.folib.constant;

import java.util.regex.Pattern;

/**
 * @author huayanjun
 * @since 2024-08-27 17:16
 */
public class DebianConstant {


    public static final String LAYOUT_NAME = "debian";

    public static final String LAYOUT_ALIAS = "debian";

    public static final String DISTRIBUTION = "distribution";

    public static final String ARCHITECTURE = "architecture";

    public static final String COMPONENT = "component";

    public static final String FILENAME = "filename";

    public static final String NAME = "name";

    public static final String DEFAULT_EXTENSION = "deb";

    public static final String ATTR_DISTRIBUTION = "deb.distribution";

    public static final String ATTR_COMPONENT = "deb.component";

    public static final String ATTR_ARCHITECTURE = "deb.architecture";




    public static final String PACKAGE_EXTENSION = "package";

    public static final String PACKAGE_PREFIX = "dists";

    public static final String DEB_PREFIX = "pool";
    public static final String EXTENSION = "extension";

    public static final String DEBIAN_DEB_REGEX = "^pool/(main|contrib|non-free)(?:-[a-z]+)?/(?:lib[a-z0-9]?|[a-z0-9])/([a-z0-9][a-z0-9+.-]*)/(?:[a-z0-9][a-z0-9+.-]*_)([0-9][0-9a-zA-Z.+:~-]*)(?:-[0-9.+~]*)?(?:_[a-z0-9]+)?\\.(?:deb|udeb)$";
    public static final Pattern PATH_PATTERN = Pattern.compile(DEBIAN_DEB_REGEX);

    public static final Pattern CUSTOM_PATTERN=Pattern.compile(".*/([^/]+)_([^_]+(?:[-+][^_]+)*)_([^_]+)\\.([a-zA-Z0-9]+)$|([^/]+)_([^_]+(?:[-+][^_]+)*)_([^_]+)\\.([a-zA-Z0-9]+)$");

    public static final String DEBIAN_PACKAGE_REGEX="dists/(?<codename>[a-zA-Z0-9\\-_]+)/(?<component>[a-zA-Z0-9\\-_]+)/binary-(?<architecture>[a-zA-Z0-9\\-_]+)/(?<filename>Packages(?:\\.(gz|bz2|xz|lzma))?)";

    public static final Pattern PACKAGE_PATTERN = Pattern.compile(DEBIAN_PACKAGE_REGEX);

    public static final Pattern META_PATTERN=Pattern.compile("^(Packages.*|.*\\.deb)$");
    public static final String CONTROL_FILENAME="Filename";
    public static final String CONTROL_SHA256="SHA256";

    public static final String CONTROL_MD5SUM="MD5sum";
    public static final String CONTROL_SIZE="Size";




}
