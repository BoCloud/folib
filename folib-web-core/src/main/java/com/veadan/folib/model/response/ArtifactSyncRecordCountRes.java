package com.veadan.folib.model.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class ArtifactSyncRecordCountRes {

    private Long successCount;

    private Long failedCount;

    private Long totalCount;

    private BigDecimal fileSizeCount;

    private String date;
}
