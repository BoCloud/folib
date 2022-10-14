package com.veadan.folib.storage;

import com.fasterxml.jackson.annotation.JsonRootName;

import javax.annotation.concurrent.Immutable;
import javax.validation.constraints.NotBlank;
import java.io.Serializable;

/**
 * @author leipenghui
 */
@Immutable
@JsonRootName("vulnerabilities")
public class VulnerabilitiesDto
        implements Serializable {

    /**
     * 白名单列表
     */
    @NotBlank(message = "请填写白名单", groups = {WhiteGroup.class})
    private String white;

    /**
     * 黑名单列表
     */
    @NotBlank(message = "请填写黑名单", groups = {BlackGroup.class})
    private String black;

    public String getWhite() {
        return white;
    }

    public void setWhite(String white) {
        this.white = white;
    }

    public String getBlack() {
        return black;
    }

    public void setBlack(String black) {
        this.black = black;
    }

    public VulnerabilitiesDto() {
    }

    public VulnerabilitiesDto(final VulnerabilitiesDto source) {
        this.white = source.getWhite();
        this.black = source.getBlack();
    }

    public interface WhiteGroup
            extends Serializable {
        // 白名单组
    }

    public interface BlackGroup
            extends Serializable {
        // 白名单组
    }

}
