package com.veadan.folib.domain;

import com.veadan.folib.constant.DebianConstant;
import com.veadan.folib.util.DebianUtils;
import lombok.Data;

/**
 * @author veadan
 * @since 2024-09-02 16:28
 */
@Data
public class DebianPackagesContext {

    private final String distribution;
    private final String component;
    private final String architecture;
    private final String binaryPath;

    private final boolean automaticLayout;

    public DebianPackagesContext(String distribution, String component, String architecture) {
        this.distribution = distribution;
        this.component = component;
        this.architecture = architecture;
        this.automaticLayout = DebianUtils.allAreNotBlank(distribution, component, architecture);
        if (!this.automaticLayout && !DebianUtils.allAreBlank(distribution, component, architecture)) {
            throw new IllegalArgumentException(String.format("All Debian coordinates must be specified: %s/%s/%s", distribution, component, architecture));
        } else {
            this.binaryPath = this.calcBinaryPath();
        }
    }



    private String calcBinaryPath() {
        return this.automaticLayout ? String.format("%s/%s/%s/binary-%s", DebianConstant.PACKAGE_PREFIX, this.distribution, this.component, this.architecture) : "";
    }
}
