package com.veadan.folib.constants;

import com.google.common.collect.Lists;

import java.util.List;

/**
 * @author leipenghui
 **/
public interface NpmConstants {

    /**
     * npm 包索引文件存储路径
     */
    String PACKAGE_JSON_PATH = ".npm/";

    /**
     * NPM tgz
     */
    String EXTENSION_TGZ = "tgz";

    /**
     * NPM har
     */
    String EXTENSION_HAR = "har";

    /**
     * NPM 仓库兼容的后缀
     */
    List<String> EXTENSION_LIST = Lists.newArrayList("tgz", "har");

    /**
     * NPM 仓库兼容的后缀
     */
    List<String> METADATA_EXTENSION_LIST = Lists.newArrayList("json", "json5");

}
