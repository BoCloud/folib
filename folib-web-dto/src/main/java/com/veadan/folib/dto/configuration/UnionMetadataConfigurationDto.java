package com.veadan.folib.dto.configuration;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;
import java.util.Objects;

/**
 * @author veadan
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UnionMetadataConfigurationDto
        implements Serializable {

    /**
     * 元数据key
     */
    @NotBlank(message = "元数据key不能为空")
    private String metadataKey;

    /**
     * 元数据value
     */
    @NotBlank(message = "元数据value不能为空")
    private String metadataValue;


    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof UnionMetadataConfigurationDto)) {
            return false;
        }
        UnionMetadataConfigurationDto that = (UnionMetadataConfigurationDto) o;
        return metadataKey.equals(that.metadataKey) &&
                metadataValue.equals(that.metadataValue);
    }

    @Override
    public int hashCode() {
        return Objects.hash(metadataKey, metadataValue);
    }
}