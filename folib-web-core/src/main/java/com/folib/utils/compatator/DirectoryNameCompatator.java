package com.folib.utils.compatator;

import com.folib.domain.FileContent;

import java.util.Comparator;

public class DirectoryNameCompatator implements Comparator<FileContent> {
    @Override
    public int compare(FileContent o1, FileContent o2) {
        // 按照名字升序
        String name1 = o1.getName();
        String name2 = o2.getName();
        return name1.compareTo(name2);
    }
}
