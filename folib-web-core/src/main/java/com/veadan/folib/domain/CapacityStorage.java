package com.veadan.folib.domain;

import lombok.*;


@EqualsAndHashCode(callSuper = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
public class CapacityStorage extends ExceedsSizeStorage {

    private String repositoryId;

    private boolean isNotice;

}
