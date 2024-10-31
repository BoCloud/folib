/**
功能：存储信息卡片信息组件
作者：张佳宁
日期：
**/
<template>
    <a-card :bordered="false" id="profile" class="card-profile-head" :bodyStyle="{ padding: 0, }">
        <template #title>
            <a-row type="flex" align="middle">
                <a-col :span="layoutType === 'isFilter' ? 8 : 20" class="col-info">
                    <a-avatar :title="layoutType === 'isFilter' ? $t('Storage.CreateStorageSpace') :''" :style="layoutType === 'isFilter' ? 'cursor:pointer':''" :size="74" shape="square" @click="createHandleView" src="images/folib/storage.svg" />
                    <div class="avatar-info">
                    <h4 class="font-semibold m-0">
                        <span v-if="layoutType !== 'isFilter'">{{ currentStorage.id }}</span>
                        <a-dropdown overlayClassName="overlayClassName" v-else :trigger="['click']">
                            <a style="color: #141414;" class="ant-dropdown-link" @click="e => e.preventDefault()">
                                {{ currentStorage.id }} <a-icon style="font-size:20px;" type="down" />
                            </a>
                            <a-menu slot="overlay" @click="handheTableSearch($event,'storageId')">
                                <a-menu-item v-for="item in storageData" :key="item.id" :class="{active:currentStorage.id === item.id }">
                                    {{ item.id }}
                                </a-menu-item>
                            </a-menu>
                        </a-dropdown>
                        <a-tooltip placement="topLeft">
                            <template slot="title">
                                <span>{{ $t('Storage.s3Storage') }}</span>
                            </template>
                            <a-icon style="margin-left: 15px" v-if="currentStorage.storageProvider === 's3'" type="cloud"
                                theme="filled" class="text-gray-6 text-lg" />
                        </a-tooltip>
                    </h4>
                    <p>{{ baseUrl }}api/browse/{{ currentStorage.id }} <a>
                        <a-tooltip placement="topLeft">
                            <template slot="title">
                            <span>{{ $t('Storage.CopyStorageSpacePath') }}</span>
                            </template>
                            <a-icon type="copy" @click="copy(baseUrl + 'api/browse/' + currentStorage.id)" />
                        </a-tooltip>
                        </a></p>
                    </div>
                </a-col>
                <a-col v-if="layoutType === 'isFilter'" :span="12" style="display: flex;align-items: center;">
                    <a-form layout="inline">
                        <a-form-item style="margin-top:-2px;">
                            <a-button type="primary" v-if="$store.state.user.roles.indexOf('ADMIN') > -1" @click="createHandleView"><a-icon type="plus" />{{ $t('Storage.CreateStorageSpace') }}</a-button>
                        </a-form-item>
                        <a-form-item>
                            <a-select
                                class="v-search self-icon_search"
                                style="width:160px;"
                                v-model="queryParams.layout"
                                :placeholder="$t('Storage.packageTypeQuery')"
                                @change="search"
                                show-search
                                allowClear
                                option-label-prop="label"
                            >
                                <a-select-option
                                    v-for="(item,index) in typeList"
                                    :key="index"
                                    :value="item.type"
                                    :label="item.name"
                                >
                                    <div class="option_style_item">
                                        <div class="image_item">
                                            <img :src="item.src" style="width: 100%;" alt="">  
                                        </div>
                                        <div>
                                            {{ item.name }}
                                        </div>
                                    </div>
                                </a-select-option>
                            </a-select>
                        </a-form-item>
                        <a-form-item>
                            <a-select 
                                class="v-search"
                                style="width:160px;"
                                v-model="queryParams.type" 
                                show-search
                                allowClear
                                @change="search"
                                :placeholder="$t('Storage.StrategyTypeQuery')"
                            >
                                <a-select-option value="hosted">
                                    {{ $t('Storage.Local') }}
                                </a-select-option>
                                <a-select-option value="proxy">
                                    {{ $t('Storage.Agent') }}
                                </a-select-option>
                                <a-select-option value="group">
                                    {{ $t('Storage.Combination') }}
                                </a-select-option>
                            </a-select>
                        </a-form-item>
                        <!-- <a-form-item>
                            <a-select
                                class="v-search"
                                style="width:140px;"
                                v-model="queryParams.storageId"
                                :placeholder="$t('Storage.RepositoryQuery')"
                                show-search
                                @change="handheTableSearch($event,'storageId')"
                            >
                                <a-select-option
                                    v-for="(item) in storageData"
                                    :key="item.id"
                                    :value="item.id"
                                >
                                    {{ item.id }}
                                </a-select-option>
                            </a-select>
                        </a-form-item> -->
                    </a-form>
                </a-col>
                <a-col :span="4" style="display: flex; align-items: center; justify-content: flex-end;">
                    <a-tabs v-if="layoutType === 'isFilter'" class="tabs-sliding" style="margin-top:-2px;margin-right:10px;" default-active-key="1" @change="getTabKey">
                        <a-tab-pane key="1" tab="仓库"></a-tab-pane>
                        <a-tab-pane key="2" tab="概览"></a-tab-pane>
                    </a-tabs>
                    <a-tooltip placement="topLeft">
                        <template slot="title">
                            <span>{{ $t('Storage.ModifyStorageSpace') }}</span>
                        </template>
                        <div style="margin-top:4px;" v-if="hasStoragePermission()" @click="updateHandleView">
                            <svg width="20px" height="20px" viewBox="0 0 40 40" version="1.1" xmlns="http://www.w3.org/2000/svg"
                                xmlns:xlink="http://www.w3.org/1999/xlink">
                            <title>settings</title>
                            <g stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
                                <g transform="translate(-2020.000000, -442.000000)" class="fill-dark" fill="#FFFFFF"
                                fill-rule="nonzero">
                                <g transform="translate(1716.000000, 291.000000)">
                                    <g transform="translate(304.000000, 151.000000)">
                                    <polygon class="color-background" opacity="0.596981957"
                                        points="18.0883333 15.7316667 11.1783333 8.82166667 13.3333333 6.66666667 6.66666667 0 0 6.66666667 6.66666667 13.3333333 8.82166667 11.1783333 15.315 17.6716667">
                                    </polygon>
                                    <path class="color-background"
                                        d="M31.5666667,23.2333333 C31.0516667,23.2933333 30.53,23.3333333 30,23.3333333 C29.4916667,23.3333333 28.9866667,23.3033333 28.48,23.245 L22.4116667,30.7433333 L29.9416667,38.2733333 C32.2433333,40.575 35.9733333,40.575 38.275,38.2733333 L38.275,38.2733333 C40.5766667,35.9716667 40.5766667,32.2416667 38.275,29.94 L31.5666667,23.2333333 Z"
                                        opacity="0.596981957"></path>
                                    <path class="color-background"
                                        d="M33.785,11.285 L28.715,6.215 L34.0616667,0.868333333 C32.82,0.315 31.4483333,0 30,0 C24.4766667,0 20,4.47666667 20,10 C20,10.99 20.1483333,11.9433333 20.4166667,12.8466667 L2.435,27.3966667 C0.95,28.7083333 0.0633333333,30.595 0.00333333333,32.5733333 C-0.0583333333,34.5533333 0.71,36.4916667 2.11,37.89 C3.47,39.2516667 5.27833333,40 7.20166667,40 C9.26666667,40 11.2366667,39.1133333 12.6033333,37.565 L27.1533333,19.5833333 C28.0566667,19.8516667 29.01,20 30,20 C35.5233333,20 40,15.5233333 40,10 C40,8.55166667 39.685,7.18 39.1316667,5.93666667 L33.785,11.285 Z">
                                    </path>
                                    </g>
                                </g>
                                </g>
                            </g>
                            </svg>
                        </div>
                    </a-tooltip>
                </a-col>
            </a-row>
        </template>
    </a-card>
</template>

<script>
import { isAdmin } from "@/utils/permission"
import Overview from "../../StorageMonitoring/components/Overview"
import StorageInfo from "../../StorageMonitoring/components/StorageInfo"
import typeList from "./select-type";
export default {
    components:{
        Overview,
        StorageInfo
    },
    props:['currentStorage','baseUrl','layoutType','storageData'],
    data() {
        return {
            queryParams:{
                storageId:'',
                layout: undefined,
                type: undefined,
                limit:1000,
                page:1
            },
        }
    },
    watch:{
        currentStorage: {
            handler(val){
                if(val){
                    this.queryParams.storageId = val.id
                }
            },
            immediate:true,
            deep:true,
            typeList:[]
        }
    },
    mounted() {
        this.$nextTick
        this.typeList = typeList
    },
    methods:{
        emptyQuery(){
            this.queryParams.layout = undefined
            this.queryParams.type = undefined
        },
        search(){
            this.handheTableSearch()
        },
        hasStoragePermission() {
            return isAdmin() || this.currentStorage.admin === this.$store.state.user.name
        },
        updateHandleView(){
            this.$emit('updateHandleView')
        },
        copy(val){
            this.$emit('copy',val)
        },
        createHandleView(){
            if(this.$store.state.user.roles.indexOf('ADMIN') > -1){
                this.$emit('createHandleView')
            }
        },
        handheTableSearch(val,type){
            if(type){
                this.$emit('handheTableSearch',val.key, type,this.queryParams)
            }else{
                this.$emit('handheTableSearch',this.queryParams)
            }
        },
        getTabKey(val){
            this.$emit('showOverview',val)
        }
    }
}
</script>
<style lang="scss">
    .overlayClassName{
        height:500px;
        overflow: auto;
    }
    .active{
        background: #bae7ff;
        font-weight: 600;
    }
    .ant-dropdown-menu-item:hover{
        background: #bae7ff;
    }
    .option_style_item{
        display:flex;
        justify-content:space-between;
    }
    .image_item{
        border-radius: 4px; 
        width:25px;
        padding: 1px;
        background-image: linear-gradient( 310deg, #020202, #5c6391 );
    }
</style>
