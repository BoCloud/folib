package com.veadan.folib.artifact.coordinates;

import java.net.URI;

import javax.xml.bind.annotation.XmlRootElement;

/**
 * @author Veadan
 * @author xuxinping
 */
@XmlRootElement(name = "artifactCoordinates")
public interface ArtifactCoordinates<C extends ArtifactCoordinates<C, V>, V extends Comparable<V>>
        extends Comparable<C>, GenericArtifactCoordinates
{

    String getId();

    V getNativeVersion();

    String buildPath();

    URI buildResource();

    URI buildLayoutResource();
}
