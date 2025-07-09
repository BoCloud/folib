/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
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
