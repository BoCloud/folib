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
package com.folib.aql.utils;

import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class TreeUtil {

    public  List<FileObj> toTree(Set<String> set){
        List<FileObj> res = new ArrayList<>();
        for (String s : set) {
            str2List(s, res);
        }
        return res;
    }

    private  void str2List(String str, List<FileObj> list) {
        if (str.contains("/")) {
            String current = str.split("/")[0];
            FileObj fileObj = contains(list, current);
            if (ObjectUtils.isEmpty(fileObj)) {
                list.add(new FileObj() {{
                    setName(current);
                    setType("DIR");
                    setIsLeaf(false);
                    ArrayList<FileObj> objects = new ArrayList<>();
                    str2List(str.replaceFirst(current + "/", ""), objects);
                    setChildren(objects);
                }});
            } else {
                str2List(str.replaceFirst(current + "/", ""), fileObj.getChildren());
            }
        } else {
            list.add(new FileObj() {{
                setName(str);
                setType("FILE");
                setIsLeaf(true);
            }});
        }
    }

    private static FileObj contains(List<FileObj> list, String str) {
        for (FileObj fileObj : list) {
            if (fileObj.getName().equals(str)) return fileObj;
        }
        return null;
    }


    class FileObj {
        private String name;
        private String type;
        private boolean isLeaf;

        public boolean getIsLeaf() {
            return isLeaf;
        }

        public void setIsLeaf(boolean isLeaf) {
            this.isLeaf = isLeaf;
        }

        private List<FileObj> children;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getType() {
            return type;
        }

        public void setType(String type) {
            this.type = type;
        }

        public List<FileObj> getChildren() {
            return children;
        }

        public void setChildren(List<FileObj> children) {
            this.children = children;
        }
    }
}
