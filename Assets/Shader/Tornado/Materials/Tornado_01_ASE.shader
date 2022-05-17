// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Vincent/Tornado_01_ASE"
{
	Properties
	{
		[HideInInspector] __dirty( "", Int ) = 1
	}

	SubShader
	{
		Tags{ "RenderType" = "Opaque"  "Queue" = "Geometry+0" }
		Cull Back
		CGPROGRAM
		#pragma target 3.0
		#pragma surface surf Standard keepalpha addshadow fullforwardshadows 
		struct Input
		{
			half filler;
		};

		void surf( Input i , inout SurfaceOutputStandard o )
		{
			o.Alpha = 1;
		}

		ENDCG
	}
	Fallback "Diffuse"
	CustomEditor "ASEMaterialInspector"
}
/*ASEBEGIN
Version=18935
0;73;1600;748;5591.752;621.8632;2.09378;True;False
Node;AmplifyShaderEditor.CommentaryNode;46;-5019.464,-896.262;Inherit;False;2261.371;719.4493;Comment;17;33;32;31;30;36;41;38;40;39;37;42;35;43;45;34;29;44;Freeze;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;28;-2538.539,60.8515;Inherit;False;1280.806;538.9435;Comment;6;21;22;23;25;26;27;Ice Emssion;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;20;-2550.499,-896.3124;Inherit;False;1705.697;734;Comment;11;5;6;7;8;17;18;11;12;16;14;13;Fire Emssion;1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleAddOpNode;38;-3997.623,-341.8126;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.1;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;40;-4091.427,-541.16;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StepOpNode;39;-3733.819,-445.8127;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StepOpNode;37;-3872.719,-822.113;Inherit;True;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;42;-3552.826,-846.262;Inherit;False;_Freeze;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;35;-4513.623,-292.8126;Inherit;False;Property;_Height;Height;5;0;Create;True;0;0;0;False;0;False;25;0;0;40;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;43;-3254.885,-628.812;Inherit;True;2;2;0;FLOAT;0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;45;-2982.093,-645.0262;Inherit;False;_FreezeBorder;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.WorldPosInputsNode;34;-4419.623,-504.8126;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RangedFloatNode;29;-4964.872,-832.8573;Inherit;False;Property;_FrostLevel;Frost Level;4;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;44;-3555.216,-395.812;Inherit;False;Property;_FreezeBorderColor;Freeze Border Color;6;1;[HDR];Create;True;0;0;0;False;0;False;1,1,1,1;1,1,1,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TextureCoordinatesNode;47;-4964.434,21.91418;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.Vector2Node;48;-4938.661,202.6416;Inherit;False;Constant;_OpacityPanning;OpacityPanning;7;0;Create;True;0;0;0;False;0;False;0.6,0.6;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.GetLocalVarNode;49;-4953.661,379.6416;Inherit;False;42;_Freeze;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;50;-4708.661,259.6416;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.PannerNode;51;-4523.661,44.6416;Inherit;False;3;0;FLOAT2;0,0;False;2;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SamplerNode;52;-4294.661,11.6416;Inherit;True;Property;_OpacityMainTex;OpacityMainTex;7;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;53;-3798.661,-10.3584;Inherit;False;2;2;0;FLOAT;0.01;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;55;-4302.661,292.6416;Inherit;False;Property;_OpacityMultiplyer;OpacityMultiplyer;8;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.VertexColorNode;56;-4238.661,421.6416;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleSubtractOpNode;41;-3533.225,-622.4592;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;36;-4183.623,-417.8126;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;30;-4969.464,-705.7142;Inherit;True;Property;_IceLevelOffset;Ice Level Offset;3;0;Create;True;0;0;0;False;0;False;-1;115a8fd51d9c61c4abc4daf7afd5a13b;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleAddOpNode;31;-4582.623,-828.8127;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;18;-1079.803,-575.3124;Inherit;True;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;14;-1332.803,-442.312;Inherit;False;2;2;0;FLOAT;0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.ColorNode;27;-1848.35,387.795;Inherit;False;Constant;_IceColor;IceColor;3;1;[HDR];Create;True;0;0;0;False;0;False;0.380429,0.8919811,0.9716981,1;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;26;-1492.733,226.0401;Inherit;True;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SamplerNode;25;-1928.962,185.5501;Inherit;True;Property;_IceMainTex;IceMainTex;2;0;Create;True;0;0;0;False;0;False;-1;115a8fd51d9c61c4abc4daf7afd5a13b;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.PannerNode;23;-2149.856,215.0899;Inherit;False;3;0;FLOAT2;0,0;False;2;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.Vector2Node;22;-2488.539,299.0657;Inherit;False;Constant;_IcePanning;IcePanning;2;0;Create;True;0;0;0;False;0;False;-0.01,-0.02;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.TextureCoordinatesNode;21;-2488.539,110.8515;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;13;-1311.803,-688.3124;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;54;-3896.661,260.6416;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.ColorNode;16;-1677.803,-374.3121;Inherit;False;Constant;_FireFirstColor;FireFirstColor;1;1;[HDR];Create;True;0;0;0;False;0;False;0.9528302,0.3710616,0.1483179,1;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.StepOpNode;11;-1644.5,-655.8804;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;17;-1632.803,-846.3124;Inherit;False;Property;_FireSecondColor;FireSecondColor;1;1;[HDR];Create;True;0;0;0;False;0;False;0.3109917,1,0.03301889,1;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;8;-1971.5,-607.8802;Inherit;True;Property;_FireMainTex;FireMainTex;0;0;Create;True;0;0;0;False;0;False;-1;67513695329500347a2fe34cfaf22488;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.PannerNode;7;-2194.5,-595.8802;Inherit;False;3;0;FLOAT2;0,0;False;2;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.Vector2Node;6;-2476.499,-463.8799;Inherit;False;Constant;_FirePanning;FirePanning;0;0;Create;True;0;0;0;False;0;False;-0.6,-0.6;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.TextureCoordinatesNode;5;-2500.499,-619.8804;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TFHCRemapNode;33;-4335.623,-810.8127;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;-0.1;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;32;-4599.623,-636.8126;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StepOpNode;12;-1634.531,-498.3113;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.3;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;57;-3676.661,361.6416;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.StandardSurfaceOutputNode;19;0,0;Float;False;True;-1;2;ASEMaterialInspector;0;0;Standard;Vincent/Tornado_01_ASE;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;Back;0;False;-1;0;False;-1;False;0;False;-1;0;False;-1;False;0;Opaque;0.5;True;True;0;False;Opaque;;Geometry;All;18;all;True;True;True;True;0;False;-1;False;0;False;-1;255;False;-1;255;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;False;2;15;10;25;False;0.5;True;0;0;False;-1;0;False;-1;0;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;0;0,0,0,0;VertexOffset;True;False;Cylindrical;False;True;Relative;0;;-1;-1;-1;-1;0;False;0;0;False;-1;-1;0;False;-1;0;0;0;False;0.1;False;-1;0;False;-1;False;16;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT;0;False;9;FLOAT;0;False;10;FLOAT;0;False;13;FLOAT3;0,0,0;False;11;FLOAT3;0,0,0;False;12;FLOAT3;0,0,0;False;14;FLOAT4;0,0,0,0;False;15;FLOAT3;0,0,0;False;0
WireConnection;38;0;36;0
WireConnection;40;0;33;0
WireConnection;39;0;40;0
WireConnection;39;1;38;0
WireConnection;37;0;33;0
WireConnection;37;1;36;0
WireConnection;42;0;37;0
WireConnection;43;0;41;0
WireConnection;43;1;44;0
WireConnection;45;0;43;0
WireConnection;50;0;48;0
WireConnection;50;1;49;0
WireConnection;51;0;47;0
WireConnection;51;2;50;0
WireConnection;52;1;51;0
WireConnection;53;1;52;1
WireConnection;41;0;39;0
WireConnection;41;1;37;0
WireConnection;36;0;34;2
WireConnection;36;1;35;0
WireConnection;31;0;29;0
WireConnection;31;1;30;1
WireConnection;18;0;13;0
WireConnection;18;1;14;0
WireConnection;14;0;12;0
WireConnection;14;1;16;0
WireConnection;26;0;25;0
WireConnection;26;1;27;0
WireConnection;25;1;23;0
WireConnection;23;0;21;0
WireConnection;23;2;22;0
WireConnection;13;0;17;0
WireConnection;13;1;11;0
WireConnection;54;0;52;1
WireConnection;54;1;55;0
WireConnection;54;2;56;0
WireConnection;11;0;8;1
WireConnection;8;1;7;0
WireConnection;7;0;5;0
WireConnection;7;2;6;0
WireConnection;33;0;31;0
WireConnection;33;4;32;0
WireConnection;32;2;29;0
WireConnection;12;0;8;1
WireConnection;57;0;54;0
ASEEND*/
//CHKSM=1F8F765D5818354722FE30E011E710E724F618AF