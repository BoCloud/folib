package com.veadan.folib.forms;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ArtifactMetadataBatchForm {
    private List<ArtifactMetadataForm> list;
}
