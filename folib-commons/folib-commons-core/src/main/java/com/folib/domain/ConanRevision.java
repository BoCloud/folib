package com.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2024/3/14
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ConanRevision {

    /**
     * revision
     */
    private String revision;

    /**
     * time
     */
    private String time;
}
