package com.folib.artifact.coordinates;

import com.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.LayoutCoordinatesAdapter;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 * @date 2023/8/1 16:00
 */
@Component
public class CocoapodsCoordinatesAdapter extends
        LayoutCoordinatesAdapter<CocoapodsCoordinates, String> {

    public CocoapodsCoordinatesAdapter() {
        super(Vertices.COCOAPODS_COORDINATES, CocoapodsCoordinates.class);

    }

    @Override
    protected CocoapodsCoordinates newInstance() {
        return new CocoapodsCoordinates();
    }
}
