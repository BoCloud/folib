package com.veadan.folib.forms.configuration;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;
import java.util.Objects;

/**
 * @author leipenghui
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UnionMetadataConfigurationForm
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
        if (!(o instanceof UnionMetadataConfigurationForm)) {
            return false;
        }
        UnionMetadataConfigurationForm that = (UnionMetadataConfigurationForm) o;
        return metadataKey.equals(that.metadataKey) &&
                metadataValue.equals(that.metadataValue);
    }

    @Override
    public int hashCode() {
        return Objects.hash(metadataKey, metadataValue);
    }
}