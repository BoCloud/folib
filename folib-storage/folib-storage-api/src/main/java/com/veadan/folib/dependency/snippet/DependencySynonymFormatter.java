package com.veadan.folib.dependency.snippet;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;

/**
 * @author carlspring
 */
public interface DependencySynonymFormatter
{

    void register();

    String getLayout();

    String getFormatAlias();

    String getDependencySnippet(ArtifactCoordinates coordinates);

}
