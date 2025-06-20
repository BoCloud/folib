package com.veadan.folib.dto.configuration;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.veadan.folib.configuration.AdvancedConfiguration;
import com.veadan.folib.configuration.MutableAdvancedConfiguration;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotNull;
import java.util.Optional;

/**
 * @author leipenghui
 * @date 2023/9/24
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AdvancedConfigurationDto {

    @NotNull(message = "A allowAnonymous must be specified.")
    @JsonProperty
    private Boolean allowAnonymous;

    @NotNull(message = "A showChecksum must be specified.")
    @JsonProperty
    private Boolean showChecksum;

    @JsonProperty
    private String globalS3Bucket;

    @JsonIgnore()
    public static AdvancedConfigurationDto fromConfiguration(AdvancedConfiguration source) {
        AdvancedConfiguration configuration = Optional.ofNullable(source).orElse(new AdvancedConfiguration(new MutableAdvancedConfiguration()));
        return new AdvancedConfigurationDto(configuration.isAllowAnonymous(),
                configuration.isShowChecksum(), configuration.getGlobalS3Bucket());
    }

    @JsonIgnore()
    public MutableAdvancedConfiguration getMutableProxyConfiguration() {
        return new MutableAdvancedConfiguration(this.allowAnonymous, this.showChecksum,  this.globalS3Bucket);
    }
}
