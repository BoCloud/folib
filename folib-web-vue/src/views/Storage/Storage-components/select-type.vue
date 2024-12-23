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
                    @click="toggleCheckbox(item)"
                >
                    <a-tooltip v-if="item.disabled">
                        <template slot="title">
                            {{ $t('Storage.NextVersion') }}🤝
                        </template>
                        <a-avatar 
                            :size="64" 
                            shape="square"
                            class="icon_sty"
                        >
                            <img :src="item.src" width="48" alt="">
                        </a-avatar>
                    </a-tooltip>
                    <a-avatar 
                        v-else
                        :size="64" 
                        shape="square"
                        class="icon_sty"
                    >
                        <img :src="item.src" width="48" alt="">
                    </a-avatar>
                </div>
                <h6>{{ item.name }}</h6>
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
            typeList,
        }
    },
    mounted() {
        
    },
    methods:{
        toggleCheckbox(item) {
            if(!item.disabled){
                this.$emit('toggleCheckbox',item.type)
            }
        },
    }
}
</script>
<style lang="scss">
    .checkbox-group{
        position: relative;
    }
    .checkbox-label{
        border: none !important;
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
    .icon_sty{
        border-radius: 8px; 
        background-image: linear-gradient(310deg, rgb(250, 251, 252), rgb(221, 221, 221));
        box-shadow: 0px 0px 6px 2px rgba(0, 0, 0, 0.1);
    }
</style>