package com.veadan.folib.controllers.configuration;

import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.Views;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonRootName;
import com.fasterxml.jackson.annotation.JsonView;

/**
 * @author veadan
 */
@JsonRootName("storages")
public class StoragesOutput
{

    @JsonView(Views.ShortStorage.class)
    private List<Storage> storages;

    public StoragesOutput()
    {
    }

    public StoragesOutput(final List<Storage> storages)
    {
        this.storages = storages;
    }

    public List<Storage> getStorages()
    {
        return storages;
    }

    public void setStorages(final List<Storage> storages)
    {
        this.storages = storages;
    }
}
