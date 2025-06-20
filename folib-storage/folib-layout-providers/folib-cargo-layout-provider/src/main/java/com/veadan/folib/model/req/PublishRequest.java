package com.veadan.folib.model.req;

import com.veadan.folib.model.CargoMetadata;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class PublishRequest {

    CargoMetadata metadata;

    byte[] crateFile;

    public PublishRequest(CargoMetadata metadata, byte[] crateFile) {
        this.metadata = metadata;
        this.crateFile = crateFile;
    }
}
