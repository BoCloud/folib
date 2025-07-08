package com.folib.scanner.utils;

import org.cyclonedx.model.Bom;
import org.cyclonedx.model.Component;
import org.cyclonedx.model.vulnerability.Vulnerability;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * 针对 cyclonedx 1.5 版本进行
 *
 * @author pj
 */
public class CycloneDxUtls {

    public static void mergeFoEyesCycloneDx(Bom eyesBom, Bom scaBom) {
        if (eyesBom.getVulnerabilities() == null || eyesBom.getVulnerabilities().isEmpty()) {
            return;
        }
        Map<String, String> componentsMap = new HashMap<>();
        eyesBom.getComponents().forEach(component ->
                componentsMap.putIfAbsent(component.getBomRef(), component.getPurl()));

        Map<String, String> scaComponentMap = new HashMap<>();
        scaBom.getComponents().forEach(component ->
                scaComponentMap.putIfAbsent(component.getPurl(), component.getBomRef()));
        // 通过 entrySet 直接操作键值对，避免重复哈希计算
        for (Map.Entry<String, String> entry : componentsMap.entrySet()) {
            String purl = entry.getValue();
            // 仅在目标映射存在有效值时才覆盖，防止 null 污染
            if (scaComponentMap.containsKey(purl)) {
                entry.setValue(scaComponentMap.get(purl));
            }
        }
        mergeFoeyesVulnerability(eyesBom.getVulnerabilities(), componentsMap);
        scaBom.setVulnerabilities(Collections.unmodifiableList(eyesBom.getVulnerabilities()));
    }

    public static void mergeQanxinCycloneDx(Bom qanxinBom, Bom scaBom) {
        if (qanxinBom.getVulnerabilities().isEmpty()) {
            return;
        }
        Map<String, String> componentsMap = new HashMap<>();
        qanxinBom.getComponents().forEach(component ->
                componentsMap.putIfAbsent(component.getName()+"-"+component.getVersion(), component.getPurl()));

        Map<String, String> scaComponentMap = new HashMap<>();
        scaBom.getComponents().forEach(component ->
                scaComponentMap.putIfAbsent(component.getPurl(), component.getBomRef()));

        // 通过 entrySet 直接操作键值对，避免重复哈希计算
        for (Map.Entry<String, String> entry : componentsMap.entrySet()) {
            String purl = entry.getValue();
            // 仅在目标映射存在有效值时才覆盖，防止 null 污染
            if (scaComponentMap.containsKey(purl)) {
                entry.setValue(scaComponentMap.get(purl));
            }
        }
        mergeQanxinComponent(qanxinBom, scaBom);
        mergeVulnerabilityList(qanxinBom.getVulnerabilities(),
                affect -> mergeQianxinVulnerabilityRef(affect, componentsMap));
        scaBom.setVulnerabilities(Collections.unmodifiableList(qanxinBom.getVulnerabilities()));
    }

    public static void mergeQanxinComponent(Bom qanxinBom, Bom scaBom) {
        Map<String, Component> scaComponentMap = scaBom.getComponents().stream()
                .collect(Collectors.toMap(Component::getPurl, Function.identity()));
        scaBom.getComponents().stream()
                .map(Component::getPurl)
                .map(scaComponentMap::get)
                .filter(Objects::nonNull)
                .forEach(target -> target.getProperties().addAll(
                        qanxinBom.getComponents().stream()
                                .filter(q -> q.getPurl().equals(target.getPurl()))
                                .findFirst()
                                .map(Component::getProperties)
                                .orElse(Collections.emptyList())
                ));
        scaBom.getComponents().forEach(component -> component.setProperties(component.getProperties().stream().distinct().collect(Collectors.toList())));
    }

    private static void mergeVulnerabilityList(List<Vulnerability> vulnerabilities,
                                               Function<Vulnerability.Affect, Vulnerability.Affect> processor) {
        vulnerabilities.forEach(vuln -> {
            List<Vulnerability.Affect> result = vuln.getAffects().stream()
                    .map(affect -> {
                        if (affect.getRef() == null) return null;
                        return processor.apply(affect);
                    })
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
            vuln.setAffects(Collections.unmodifiableList(result));
        });
    }

    public static Vulnerability.Affect mergeQianxinVulnerabilityRef(Vulnerability.Affect affect,
                                                                    Map<String, String> componentsMap) {
        return componentsMap.entrySet().stream()
                .filter(entry -> affect.getRef().endsWith(entry.getKey()))
                .findFirst()
                .map(entry -> {
                    Vulnerability.Affect result = new Vulnerability.Affect();
                    result.setRef(entry.getValue());
                    Optional.ofNullable(affect.getVersions()).ifPresent(result::setVersions);
                    return result;
                })
                .orElse(null);
    }

    public static void mergeFoeyesVulnerability(List<Vulnerability> vulnerabilities, Map<String, String> componentRefMap) {
        mergeVulnerabilityList(vulnerabilities,
                affect -> {
                    Vulnerability.Affect result = new Vulnerability.Affect();
                    result.setRef(componentRefMap.getOrDefault(affect.getRef(), affect.getRef()));
                    Optional.ofNullable(affect.getVersions()).ifPresent(result::setVersions);
                    return result;
                });
    }


}
