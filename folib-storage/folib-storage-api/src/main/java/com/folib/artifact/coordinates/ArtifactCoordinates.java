package com.folib.artifact.coordinates;

import jakarta.xml.bind.annotation.XmlRootElement;

import java.net.URI;


/**
 * @author Veadan
 * @author veadan
 */
@XmlRootElement(name = "artifactCoordinates")
public interface  ArtifactCoordinates<C extends ArtifactCoordinates<C, V>, V extends Comparable<V>>
        extends Comparable<C>, GenericCoordinates
{

    String getId();

    V getNativeVersion();

    String buildPath();

    URI buildResource();

}
