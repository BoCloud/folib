package com.veadan.folib.model;


import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.List;
import lombok.Generated;


public class CargoSearchModel {
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private List<CargoSearchEntriesModel> crates;

    private CargoSearchSummaryModel meta;

    @Generated
    public CargoSearchModel() {}

    @Generated
    public void setCrates(List<CargoSearchEntriesModel> crates) {
        this.crates = crates;
    }

    @Generated
    public void setMeta(CargoSearchSummaryModel meta) {
        this.meta = meta;
    }

    @Generated
    public List<CargoSearchEntriesModel> getCrates() {
        return this.crates;
    }

    @Generated
    public CargoSearchSummaryModel getMeta() {
        return this.meta;
    }

    public CargoSearchModel(List<CargoSearchEntriesModel> crates, int total) {
        this.crates = crates;
        this.meta = new CargoSearchSummaryModel(total);
    }
}

