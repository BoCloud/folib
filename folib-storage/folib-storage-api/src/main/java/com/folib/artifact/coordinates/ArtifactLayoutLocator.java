package com.folib.artifact.coordinates;

import java.util.HashMap;
import java.util.Map;

import org.reflections.Reflections;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * With this class you can get all available Layouts from classpath.
 * 
 * 
 * @author veadan
 * 
 * @see CoordinatesLayout
 * @see ArtifactLayoutCoordinate
 * @see ArtifactCoordinates
 * 
 */
public class ArtifactLayoutLocator
{

    private static final Logger logger = LoggerFactory.getLogger(ArtifactLayoutLocator.class);

    private volatile static Map<String, ArtifactLayoutDescription> layoutByAliasEntityMap;
    private volatile static Map<String, ArtifactLayoutDescription> layoutByNameEntityMap;

    public static Map<String, ArtifactLayoutDescription> getLayoutEntityMap()
    {
        if (layoutByAliasEntityMap != null)
        {
            return layoutByAliasEntityMap;
        }
        
        locateAvaliableLayouts();
        
        return layoutByAliasEntityMap;
    }

    public static Map<String, ArtifactLayoutDescription> getLayoutByNameEntityMap()
    {
        if (layoutByNameEntityMap != null)
        {
            return layoutByNameEntityMap;
        }
        
        locateAvaliableLayouts();
        
        return layoutByNameEntityMap;
    }
    
    private static synchronized void locateAvaliableLayouts()
    {
        Map<String, ArtifactLayoutDescription> layoutByAliasEntityMapLocal = new HashMap<>();
        Map<String, ArtifactLayoutDescription> layoutByNameEntityMapLocal = new HashMap<>();

        Reflections reflections = new Reflections("com.folib.artifact.coordinates");
        for (Class<? extends ArtifactCoordinates> artifactCoordinatesClass : reflections.getSubTypesOf(ArtifactCoordinates.class))
        {
            CoordinatesLayout artifactLayout = artifactCoordinatesClass.getAnnotation(CoordinatesLayout.class);
            if (artifactLayout == null)
            {
                logger.warn("[{}] should be provided for [{}] class",
                            CoordinatesLayout.class.getSimpleName(),
                            artifactCoordinatesClass.getSimpleName());
                continue;
            }

            ArtifactLayoutDescription layoutDesc = ArtifactLayoutDescription.parse(artifactCoordinatesClass);
            layoutByAliasEntityMapLocal.put(layoutDesc.getLayoutAlias(), layoutDesc);
            layoutByNameEntityMapLocal.put(layoutDesc.getLayoutName(), layoutDesc);
        }

        layoutByAliasEntityMap = layoutByAliasEntityMapLocal;
        layoutByNameEntityMap = layoutByNameEntityMapLocal;
    }

}
