package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ColumnInfo {

    /**
     * title
     */
    private String title;

    /**
     * dataIndex
     */
    private String dataIndex;

    /**
     * key
     */
    private String key;
}
