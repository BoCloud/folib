package com.veadan.folib.metadata.model;

import lombok.Data;

@Data
public class RepomdMetadata {

    private XmlData other;
    private XmlData primary;


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
