package com.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 * @date 2023/6/20
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class DockerCatalog {

    /**
     * repositories
     */
    private List<String> repositories;

    /**
     * next
     */
    private String next;
}
