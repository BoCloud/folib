<template>
	<div class="artifact-graph">
		<div class="artifact-graph-content" id="artifactGraph" ref="artifactGraph"></div>
	</div>
</template>

<script>
import {
	getArtifactGraph
} from "@/api/module";
import G6 from '@antv/g6';

export default ({
  props: {
    component: {
      type: Object,
      default: {},
    },
  },
	watch: {
    component: {
      handler(newVal, oldVal) {
        this.initGraph();
      },
      deep: true, // 开启深度监听
    },
  },
  mounted() {
    this.initGraph();
  },
  beforeDestroy: function () {
    this.graph.destroy();
  },
	data() {
		return {
			graph: null,
			graphData: {},
		}
	},
	methods: {
		initGraph() {
			if (this.graph) {
				//如果存在画布，销毁画布，重新渲染
				this.graph.destroy()
			}
			this.artifactGraph()
		},
		artifactGraph() {
			if (!this.component.uuid) {
				return
			}
			getArtifactGraph({ componentUuid: this.component.uuid}).then(res => {
				this.graphData = res
				this.configGraph()
			});
		},
		configGraph() {
			//组件props
			const props = {
				//数据
				data: this.graphData,
				config: {
					padding: [20, 50],
					defaultLevel: 3,
					defaultZoom: 0.8,
					//缩放和拖拽
					modes: { default: ['zoom-canvas', 'drag-canvas'] },
				},
			}
			//宽高
			const container = document.getElementById('artifactGraph');
			const width = container.scrollWidth;
			const height = container.scrollHeight || 500;
			//颜色
			const colors = {
				B: '#5B8FF9',
				R: '#F46649',
				Y: '#EEBC20',
				G: '#5BD8A6',
				DI: '#A7A7A7',
			}
			// 默认配置
			const defaultConfig = {
				width,
				height,
				modes: {
					//缩放和拖拽
					default: ['zoom-canvas', 'drag-canvas'],
				},
				fitView: true,
				animate: true,
				defaultNode: {
					type: 'flow-rect',
				},
				defaultEdge: {
					type: 'cubic-horizontal',
					style: {
						stroke: '#CED4D9',
					},
				},
				layout: {
					type: 'indented',
					direction: 'LR',
					dropCap: false,
					indent: 300,
					getHeight: () => {
						return 60;
					},
				},
			}
			// 自定义节点、边
			const registerFn = () => {
				/**
				* 自定义节点
				*/
				G6.registerNode(
					'flow-rect',
					{
						shapeType: 'flow-rect',
						draw(cfg, group) {
							const {
								id,
								title,
								name,
								version,
								collapsed,
								count,
								up = '',
							} = cfg;
							const grey = '#CED4D9';
							const rectConfig = {
								width: 202,
								height: 60,
								lineWidth: 1,
								fontSize: 12,
								fill: '#fff',
								radius: 4,
								stroke: grey,
								opacity: 1,
							};

							/**
							* format the string
							* @param {string} str The origin string
							* @param {number} maxWidth max width
							* @param {number} fontSize font size
							* @return {string} the processed result
							*/
							const fittingString = (str, maxWidth, fontSize) => {
								let currentWidth = 0;
								let res = str;
								const pattern = new RegExp('[\u4E00-\u9FA5]+'); // distinguish the Chinese charactors and letters
								str.split('').forEach((letter, i) => {
									if (currentWidth > maxWidth) return;
									if (pattern.test(letter)) {
										// Chinese charactors
										currentWidth += fontSize;
									} else {
										// get the width of single letter according to the fontSize
										currentWidth += G6.Util.getLetterWidth(letter, fontSize);
									}
									if (currentWidth > maxWidth) {
										res = `${str.substr(0, i)}\n${str.substr(i)}`;
									}
								});
								return res;
							};

							const nodeOrigin = {
								x: -rectConfig.width / 2,
								y: -rectConfig.height / 2,
							};

							const textConfig = {
								textAlign: 'left',
								textBaseline: 'bottom',
							};

							const rect = group.addShape('rect', {
								attrs: {
									x: nodeOrigin.x,
									y: nodeOrigin.y,
									...rectConfig,
								},
							});

							const rectBBox = rect.getBBox();

							// title
							group.addShape('text', {
								attrs: {
									...textConfig,
									x: 12 + nodeOrigin.x,
									y: 20 + nodeOrigin.y,
									text: title.length > 28 ? title.substr(0, 28) + '...' : title,
									fontSize: 12,
									opacity: 0.85,
									fill: '#000',
									cursor: 'pointer',
								},
								name: 'name-shape',
							});

							// name
							const nameValue = group.addShape('text', {
								attrs: {
									...textConfig,
									x: 12 + nodeOrigin.x,
									y: rectBBox.maxY - 12,
									text: fittingString(name, 170, 11),
									fontSize: 10,
									fill: '#000',
									opacity: 0.85,
								},
							});

							// version
							group.addShape('text', {
								attrs: {
									...textConfig,
									x: nameValue.getBBox().maxX + 5,
									y: rectBBox.maxY - 12,
									text: version,
									fontSize: 8,
									fill: '#000',
									opacity: 0.75,
								},
							});

							let color = 'B'
							if (title === '组件信息') {
								color = 'R'
							}

							// countValue
							const countValue = group.addShape('text', {
								attrs: {
									...textConfig,
									x: rectBBox.maxX - 8,
									y: rectBBox.maxY - 12,
									text: count,
									fontSize: 12,
									textAlign: 'right',
									fill: colors[color],
								},
							});

							if (up === false) {
								let symbol = up ? 'triangle' : 'triangle-down'
								group.addShape('marker', {
									attrs: {
										...textConfig,
										x: countValue.getBBox().minX - 10,
										y: rectBBox.maxY - 12 - 6,
										symbol,
										r: 6,
										fill: colors[color],
									},
								});
							}

							// bottom line background
							const bottomBackRect = group.addShape('rect', {
								attrs: {
									x: nodeOrigin.x,
									y: rectBBox.maxY - 4,
									width: rectConfig.width,
									height: 4,
									radius: [0, 0, rectConfig.radius, rectConfig.radius],
									fill: '#E0DFE3',
								},
							});
							// bottom percent
							const bottomRect = group.addShape('rect', {
								attrs: {
									x: nodeOrigin.x,
									y: rectBBox.maxY - 4,
									width: rectBBox.width,
									height: 4,
									radius: [0, 0, 0, rectConfig.radius],
									fill: colors[color],
								},
							});

							// collapse rect
							if (cfg.children && cfg.children.length) {
								group.addShape('rect', {
									attrs: {
										x: rectConfig.width / 2 - 8,
										y: -8,
										width: 16,
										height: 16,
										stroke: 'rgba(0, 0, 0, 0.25)',
										cursor: 'pointer',
										fill: '#fff',
									},
									name: 'collapse-back',
									modelId: cfg.id,
								});

								// collpase text
								group.addShape('text', {
									attrs: {
										x: rectConfig.width / 2,
										y: -1,
										textAlign: 'center',
										textBaseline: 'middle',
										text: collapsed ? '+' : '-',
										fontSize: 16,
										cursor: 'pointer',
										fill: 'rgba(0, 0, 0, 0.25)',
									},
									name: 'collapse-text',
									modelId: cfg.id,
								});
							}
							this.drawLinkPoints(cfg, group);
							return rect;
						},
						update(cfg, item) {
							const group = item.getContainer();
							this.updateLinkPoints(cfg, group);
						},
						setState(name, value, item) {
							if (name === 'collapse') {
								const group = item.getContainer();
								const collapseText = group.find((e) => e.get('name') === 'collapse-text');
								if (collapseText) {
									if (!value) {
										collapseText.attr({
											text: '-',
										});
									} else {
										collapseText.attr({
											text: '+',
										});
									}
								}
							}
						},
						getAnchorPoints() {
							return [
								[0, 0.5],
								[1, 0.5],
							];
						},
					},
					'rect',
				);

				G6.registerEdge(
					'flow-cubic',
					{
						getControlPoints(cfg) {
							let controlPoints = cfg.controlPoints; // 指定controlPoints
							if (!controlPoints || !controlPoints.length) {
								const { startPoint, endPoint, sourceNode, targetNode } = cfg;
								const { x: startX, y: startY, coefficientX, coefficientY } = sourceNode
									? sourceNode.getModel()
									: startPoint;
								const { x: endX, y: endY } = targetNode ? targetNode.getModel() : endPoint;
								let curveStart = (endX - startX) * coefficientX;
								let curveEnd = (endY - startY) * coefficientY;
								curveStart = curveStart > 40 ? 40 : curveStart;
								curveEnd = curveEnd < -30 ? curveEnd : -30;
								controlPoints = [
									{ x: startPoint.x + curveStart, y: startPoint.y },
									{ x: endPoint.x + curveEnd, y: endPoint.y },
								];
							}
							return controlPoints;
						},
						getPath(points) {
							const path = [];
							path.push(['M', points[0].x, points[0].y]);
							path.push([
								'C',
								points[1].x,
								points[1].y,
								points[2].x,
								points[2].y,
								points[3].x,
								points[3].y,
							]);
							return path;
						},
					},
					'single-line',
				);
			}
			registerFn()
			const { data } = props;
			const initGraph = (data) => {
				if (!data) {
					return;
				}
				const { onInit, config } = props;
				const tooltip = new G6.Tooltip({
					// offsetX and offsetY include the padding of the parent container
					offsetX: 20,
					offsetY: 30,
					// the types of items that allow the tooltip show up
					// 允许出现 tooltip 的 item 类型
					itemTypes: ['node'],
					// custom the tooltip's content
					// 自定义 tooltip 内容
					getContent: (e) => {
						const outDiv = document.createElement('div');
						//outDiv.style.padding = '0px 0px 20px 0px';
						const nodeName = e.item.getModel().name;
						let formatedNodeName = '';
						for (let i = 0; i < nodeName.length; i++) {
							formatedNodeName = `${formatedNodeName}${nodeName[i]}`;
							if (i !== 0 && i % 20 === 0) formatedNodeName = `${formatedNodeName}<br/>`;
						}
						outDiv.innerHTML = `${formatedNodeName}`;
						return outDiv;
					},
					shouldBegin: (e) => {
						if (e.target.get('name') === 'name-shape') return true;
						return false;
					},
				});
				this.graph = new G6.TreeGraph({
					container: 'artifactGraph',
					...defaultConfig,
					...config,
					plugins: [tooltip],
				});
				if (typeof onInit === 'function') {
					onInit(this.graph);
				}
				this.graph.data(data);
				this.graph.render();
				this.graph.zoom(config.defaultZoom || 1);

				const handleCollapse = (e) => {
					const target = e.target;
					const id = target.get('modelId');
					const item = this.graph.findById(id);
					const nodeModel = item.getModel();
					nodeModel.collapsed = !nodeModel.collapsed;
					this.graph.layout();
					this.graph.setItemState(item, 'collapse', nodeModel.collapsed);
				};
				this.graph.on('collapse-text:click', (e) => {
					handleCollapse(e);
				});
				this.graph.on('collapse-back:click', (e) => {
					handleCollapse(e);
				});
			}
			initGraph(data);
			if (typeof window !== 'undefined')
				window.onresize = () => {
					if (!this.graph || this.graph.get('destroyed')) return;
					if (!container || !container.scrollWidth || !container.scrollHeight) return;
					this.graph.changeSize(container.scrollWidth, container.scrollHeight);
				}
		},
		successMsg(message) {
			if (!message) {
				message = "操作成功"
			}
			this.$notification["success"]({
				message: message,
			})
		},
	}
})
</script>

<style lang="scss" scoped>
$md: 768px;

.artifact-graph::v-deep {
	.artifact-graph-content {
		margin-top: 50px;
		// border: 1px solid pink;
	}

}

// .artifact-graph-content {
// 	height: 75vh;
// }

.g6-component-tooltip {
	background-color: rgba(0, 0, 0, 0.65);
	padding: 10px;
	box-shadow: rgb(174, 174, 174) 0px 0px 10px;
	width: fit-content;
	color: #fff;
	border-radius: 4px;
}
</style>