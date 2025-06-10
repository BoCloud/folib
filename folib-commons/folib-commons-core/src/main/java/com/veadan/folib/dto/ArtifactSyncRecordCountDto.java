package com.veadan.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class ArtifactSyncRecordCountDto {

    private Long successCount;

    private Long failedCount;

    private Long totalCount;

    private String date;

}
