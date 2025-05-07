package com.veadan.folib.configuration;

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.annotation.concurrent.Immutable;
import java.io.Serializable;

/**
 * @author leipenghui
 * @date 2023/9/24
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
public class AdvancedConfiguration implements Serializable {

    private boolean allowAnonymous;

    private boolean showChecksum;

    private String globalS3Bucket;

    public AdvancedConfiguration(final MutableAdvancedConfiguration delegate) {
        this.allowAnonymous = delegate.isAllowAnonymous();
        this.showChecksum = delegate.isShowChecksum();
        this.globalS3Bucket = delegate.getGlobalS3Bucket();
    }
}
