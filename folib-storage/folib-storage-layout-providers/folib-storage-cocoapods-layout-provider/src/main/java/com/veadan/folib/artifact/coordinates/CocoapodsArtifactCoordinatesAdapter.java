package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.LayoutArtifactCoordinatesEntity;
import com.veadan.folib.gremlin.adapters.LayoutArtifactCoordinatesAdapter;
import org.neo4j.ogm.annotation.NodeEntity;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/8/1 16:00
 * @since x.x.x
 */
public class CocoapodsArtifactCoordinatesAdapter extends
        LayoutArtifactCoordinatesAdapter<CocoapodsArtifactCoordinates, String> {

    public static final String LAYOUT_NAME = "CocoaPods";

    public static final String LAYOUT_ALIAS = LAYOUT_NAME;

    public CocoapodsArtifactCoordinatesAdapter(String label, Class<CocoapodsArtifactCoordinates> layoutCoordinatesClass) {
        super(Vertices.COCOAPODS_ARTIFACT_COORDINATES, CocoapodsArtifactCoordinates.class);
    }


    @Override
    protected CocoapodsArtifactCoordinates newInstance() {
        return new CocoapodsArtifactCoordinates();
    }
}
