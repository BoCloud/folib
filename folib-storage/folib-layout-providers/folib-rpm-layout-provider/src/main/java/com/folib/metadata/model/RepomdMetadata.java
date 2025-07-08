package com.folib.metadata.model;

import lombok.Data;

@Data
public class RepomdMetadata {

    private XmlData other;
    private XmlData primary;
    private XmlData filelists;

    @lombok.Data
    public static class XmlData {
        private String href;
        private String checksum;
        private long size;
        private long timestamp;
        private String openChecksum;
        private long openSize;

    }

}
