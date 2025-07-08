package com.folib.vo;

import java.util.Comparator;

public class ArtifactVersionCompatator implements Comparator<SearchArtifactInfo> {
    @Override
    public int compare(SearchArtifactInfo o1, SearchArtifactInfo o2) {
        int v1 = 0;
        int v2 = 0;
        // 降序
        try {
            v1 = Integer.valueOf(o1.getArtifactName());
            v2 = Integer.valueOf(o2.getArtifactName());
        } catch (Exception e) {
            e.printStackTrace();
            return -1;
        }
        return v2 - v1;
    }
}
