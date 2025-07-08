package com.folib.controllers.adapter.jfrog.res;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Map;

@Data
@Accessors(chain = true)
public class FileRes {

    private String uri;

    private String size;

    private String lastModified;

    private String folder;

    private String sha1;

    private Map<String, String> mdTimestamps;
}
