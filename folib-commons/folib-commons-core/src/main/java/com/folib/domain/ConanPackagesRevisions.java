package com.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ConanPackagesRevisions {

    /**
     * packageReference
     */
    private String packageReference;

    /**
     * revisions
     */
    private List<ConanRevision> revisions;
}
