package com.folib.dependency.snippet;

import com.folib.artifact.coordinates.ArtifactCoordinates;

/**
 * @author Veadan
 */
public interface DependencySynonymFormatter
{

    void register();

    String getLayout();

    String getFormatAlias();

    String getDependencySnippet(ArtifactCoordinates coordinates);

}
