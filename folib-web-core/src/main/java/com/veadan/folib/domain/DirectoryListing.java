package com.veadan.folib.domain;

import java.util.ArrayList;
import java.util.List;

public class DirectoryListing
{

    private List<FileContent> directories;

    private List<FileContent> files;


    public List<FileContent> getDirectories()
    {
        if (directories == null)
        {
            directories = new ArrayList<>();
        }

        return directories;
    }

    public List<FileContent> getFiles()
    {
        if (files == null)
        {
            files = new ArrayList<>();
        }

        return files;
    }

    public void setDirectories(List<FileContent> directories)
    {
        this.directories = directories;
    }

    public void setFiles(List<FileContent> files)
    {
        this.files = files;
    }

}
