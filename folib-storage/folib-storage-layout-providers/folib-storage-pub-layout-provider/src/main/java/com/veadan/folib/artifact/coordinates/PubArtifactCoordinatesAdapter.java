//package com.veadan.folib.artifact.coordinates;
//
//import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
//import com.veadan.folib.db.schema.Vertices;
//import com.veadan.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
//import org.springframework.stereotype.Component;
//
///**
// * @author xuxinping
// */
//@Component
//public class NpmArtifactCoordinatesAdapter
//        extends LayoutArtifactCoordinatesAdapter<PubArtifactCoordinates, SemanticVersion>
//{
//
//    public NpmArtifactCoordinatesAdapter()
//    {
//        super(Vertices.NPM_ARTIFACT_COORDINATES, PubArtifactCoordinates.class);
//    }
//
//    @Override
//    protected PubArtifactCoordinates newInstance()
//    {
//        return new PubArtifactCoordinates();
//    }
//
//}
