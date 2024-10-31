package com.veadan.folib.event;

import com.veadan.folib.domain.Artifact;
import com.veadan.folib.enums.DeltaIndexEventType;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.annotation.Nonnull;

/**
 * @author huayanjun
 * @since 2024-09-03 14:59
 */
@Data
@NoArgsConstructor
public class DebianIndexEvent {

    private Artifact artifact;
    private String distribution;
    private String component;
    private String architecture;
    private DeltaIndexEventType eventType;

    public DebianIndexEvent( Artifact artifact,DeltaIndexEventType eventType, String distribution,String component, String architecture) {
        this.artifact=artifact;
        this.eventType = eventType;
        this.distribution = distribution;
        this.component = component;
        this.architecture = architecture;
    }

    public String componentArchitectureGroup() {
        return this.component + ":" + this.architecture;
    }

    public String distributionComponentGroup() {
        return this.distribution + ":" + this.component;
    }

    public DebianIndexEvent(DebianIndexEvent other, String newArchitecture) {
        this(other.getArtifact(),other.getEventType(), other.getDistribution(), other.getComponent(), newArchitecture);
    }
}
