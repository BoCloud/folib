package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author leipenghui
 * @date 2024/3/25
 **/
@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class SearchResults {

    private List<String> results;
}
