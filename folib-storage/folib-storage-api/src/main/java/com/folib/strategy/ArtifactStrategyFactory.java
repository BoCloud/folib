package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Component
public class ArtifactStrategyFactory {

    @Resource
    private Map<String , ArtifactStrategy> strategyMap = new ConcurrentHashMap<>();

    public ArtifactCoordinates getArtifactCoordinates(Class<?> clazz , GenericCoordinatesEntity entity){
        return strategyMap.get(toInstanceVariableName(clazz.getSimpleName())).getArtifactCoordinates(entity);
    }

    public  String toInstanceVariableName(String className) {
        if (className == null || className.isEmpty()) {
            return className;
        }
        // 首字母转为小写 + 剩余子字符串
        return Character.toLowerCase(className.charAt(0)) + className.substring(1)+"Strategy";
    }

}
