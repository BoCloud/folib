/**
功能：类型选择组件
作者：张佳宁
日期：2024年10月21日17:17:47
**/
<template>
    <div class="checkbox-group">
        <div v-if="isEdit" class="disabled_box" />
        <a-row type="flex" :gutter="[50]">
            <a-col :span="4" v-for="(item,index) in typeList" :key="index">
                <div class="checkbox-label" 
                    :class="[layoutChecked === item.type ? 'active' : '']"
                    @click="toggleCheckbox(item.type)"
                >
                    <a-avatar :size="44" shape="square"
                        style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                        <img :src="item.src" style="width: 100%;" alt="">
                    </a-avatar>
                </div>
                <h6>{{ item.name }}</h6>
            </a-col>
            <a-col :span="4">
                <div class="checkbox-label" :class="[layoutChecked === 'gems' ? 'active' : '']">
                <a-tooltip>
                    <template slot="title">
                    {{ $t('Storage.NextVersion') }}🤝
                    </template>
                <a-avatar :size="44" shape="square"
                            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                    <img src="images/folib/gems.svg" style="width: 100%;" alt="">
                </a-avatar>
                </a-tooltip>
                </div>
                <h6>Gems</h6>
            </a-col>
            <a-col :span="4">
                <div class="checkbox-label" :class="[layoutChecked === 'rust' ? 'active' : '']">
                <a-tooltip>
                    <template slot="title">
                    {{ $t('Storage.NextVersion') }}🤝
                    </template>
                <a-avatar :size="44" shape="square"
                            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );">
                    <img src="images/folib/rust.svg" style="width: 100%;" alt="">
                </a-avatar>
                </a-tooltip>
                </div>
                <h6>Rust</h6>
            </a-col>
        </a-row>
    </div>
</template>

<script>
import typeList from './select-type.js'
export default {
    props:['layoutChecked','isEdit'],
    data() {
        return {
            typeList:[]
        }
    },
    mounted() {
        this.typeList = typeList
    },
    methods:{
        toggleCheckbox(item) {
            this.$emit('toggleCheckbox',item)
        },
    }
}
</script>
<style>
    .checkbox-group{
        position: relative;
    }
    .disabled_box{
        position: absolute;
        height: 100%;
        width: 100%;
        z-index:9999;
        background: #fff;
        opacity: 0.1;
        cursor: not-allowed;
    }
</style>